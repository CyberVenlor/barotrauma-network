#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Lua bundler: inline `require(...)` for every .lua under a directory.

Rules (as requested):
- For each Lua file, find all require("xxx") / require 'xxx'.
- Insert each required module ONCE, wrapped as:
    local <module_var> = (function ()
        <original module content>
    end)()
  and place inserts before the first place that needs them (we emit all inserts
  at the top in dependency order, so every dependent sees its deps above).
- Replace every require("xxx") expression with the variable <module_var>.
- Recurse: required modules may themselves require others; keep inlining until
  no inlinable requires remain.
- Output each processed Lua file to an `out` directory (which is skipped when
  scanning inputs).
- Module name resolution:
    * `require "foo"` → file named `foo.lua` if present
    * `require "a.b.c"` → file `a/b/c.lua` if present
    * Also falls back to any file whose stem equals the last segment (e.g. `c.lua`)
- Variable name used is a sanitized version of the require string:
    a.b.c  -> a_b_c
    foo-bar-> foo_bar
Notes:
- This is a pragmatic regex-based approach (does not parse Lua). It ignores
  requires inside long strings/comments. Works well for typical module-style code.
- Cyclic requires are not fully resolved; order is DFS (deps first). Most Lua
  modules that only define functions will be fine; top-level side effects across
  cycles may need manual care.
"""

import os
import re
import sys
import argparse
from collections import OrderedDict, defaultdict
from pathlib import Path
from typing import Dict, List, Tuple, Set, Optional

def _is_ident_char(c: str) -> bool:
    return c.isalnum() or c == '_'

def _long_open(s: str, i: int) -> tuple[bool, int, int]:
    # s[i] == '['，返回(是否长括号, 等号数量, 内容起始索引)
    n = len(s)
    j = i + 1
    eq = 0
    while j < n and s[j] == '=':
        eq += 1
        j += 1
    if j < n and s[j] == '[':
        return True, eq, j + 1
    return False, 0, i

def lua_minify(src: str) -> str:
    # 去掉：所有注释，所有缩进/回车；把任意连续空白合并成“必要时的一个空格”
    # 保留：普通字符串('"/转义)、长字符串([==[ ... ]==]) 原样
    out = []
    i, n = 0, len(src)

    NORMAL, LINE_CMT, BLOCK_CMT, STR_SQ, STR_DQ, LONG_STR, LONG_CMT = range(7)
    st = NORMAL
    long_eq = 0  # 对应 [==[ 的等号个数

    def last_non_space():
        for k in range(len(out) - 1, -1, -1):
            if not out[k].isspace():
                return out[k]
        return ''

    while i < n:
        c = src[i]

        if st == NORMAL:
            # 注释：--... 或 --[=*[...]=*]
            if c == '-' and i + 1 < n and src[i + 1] == '-':
                i += 2
                if i < n and src[i] == '[':
                    ok, eq, j = _long_open(src, i)
                    if ok:
                        st, long_eq = LONG_CMT, eq
                        i = j
                        continue
                st = LINE_CMT
                continue

            # 长字符串：[=*[ ... ]=*]
            if c == '[':
                ok, eq, j = _long_open(src, i)
                if ok:
                    out.append(src[i:j])  # 写入开括号含等号
                    st, long_eq = LONG_STR, eq
                    i = j
                    continue

            # 普通字符串
            if c == "'":
                st = STR_SQ
                out.append(c); i += 1
                continue
            if c == '"':
                st = STR_DQ
                out.append(c); i += 1
                continue

            # 空白折叠
            if c.isspace():
                j = i
                while j < n and src[j].isspace():
                    j += 1
                prev = last_non_space()
                nxt = src[j] if j < n else ''
                if _is_ident_char(prev) and _is_ident_char(nxt):
                    out.append(' ')
                i = j
                continue

            # 其它字面量
            out.append(c)
            i += 1
            continue

        if st == LINE_CMT:
            # 吃到换行（换行本身也丢掉）
            if c == '\n':
                st = NORMAL
            i += 1
            continue

        if st in (LONG_CMT, LONG_STR):
            if c == ']':
                j = i + 1
                k = j
                while k < n and k - j < long_eq and src[k] == '=':
                    k += 1
                if (k - j) == long_eq and k < n and src[k] == ']':
                    if st == LONG_STR:
                        out.append(src[i:k + 1])
                    # 结束长注释/长字符串
                    st = NORMAL
                    i = k + 1
                    continue
            if st == LONG_STR:
                out.append(c)
            # LONG_CMT 不写出
            i += 1
            continue

        if st in (STR_SQ, STR_DQ):
            out.append(c)
            if c == '\\' and i + 1 < n:
                out.append(src[i + 1])
                i += 2
                continue
            if (st == STR_SQ and c == "'") or (st == STR_DQ and c == '"'):
                st = NORMAL
            i += 1
            continue

    return ''.join(out)

# --- Regex to find require("...") or require '...'
REQ_RE = re.compile(
    r'''(?x)               # verbose
        \brequire          # require
        \s* \(
            \s* (["'])     # opening quote in group 1
            (?P<name>[^"']+)
            \1 \s*
        \)
        |
        \brequire          # or: require '...'
        \s* (["'])         # opening quote (group 2)
            (?P<name2>[^"']+)
        \2
    '''
)

# Recognize .lua files
def is_lua_file(path: Path) -> bool:
    return path.is_file() and path.suffix.lower() == ".lua"

def sanitize_var(modname: str) -> str:
    # Turn into a valid Lua identifier (best-effort)
    s = re.sub(r'[^0-9A-Za-z_]', '_', modname)
    # Lua identifiers cannot start with a digit -> prefix underscore if needed
    if re.match(r'^\d', s):
        s = "_" + s
    return s

def find_requires(text: str) -> List[str]:
    found: List[str] = []
    for m in REQ_RE.finditer(text):
        name = m.group('name') or m.group('name2')
        if name is not None:
            found.append(name)
    return found

def replace_requires(text: str, known_map: Dict[str, str]) -> str:
    # known_map: require-string -> lua-variable-name to replace with
    def _repl(m: re.Match) -> str:
        name = m.group('name') or m.group('name2')
        if name is None:
            return m.group(0)
        if name in known_map:
            return known_map[name]
        return m.group(0)
    return REQ_RE.sub(_repl, text)

def read_text(p: Path) -> str:
    return p.read_text(encoding="utf-8")

def write_text(p: Path, s: str):
    p.parent.mkdir(parents=True, exist_ok=True)
    p.write_text(s, encoding="utf-8")

def indent_block(s: str, spaces: int = 4) -> str:
    pad = " " * spaces
    # Keep trailing newline behavior stable
    if not s.endswith("\n"):
        s = s + "\n"
    return "".join(pad + line if line.strip() != "" else line for line in s.splitlines(True))

# Build a module index from source directory:
# Returns:
#   mod_by_require: mapping from require-string -> Path
#   aliases: for a given Path, list of require-keys that map to it (for info)
def build_module_index(src_dir: Path, out_dir: Path) -> Tuple[Dict[str, Path], Dict[Path, List[str]]]:
    mod_by_require: Dict[str, Path] = {}
    aliases: Dict[Path, List[str]] = defaultdict(list)

    # Helper to register a key if not already taken
    def register(key: str, path: Path):
        if key not in mod_by_require:
            mod_by_require[key] = path
            aliases[path].append(key)

    for root, dirs, files in os.walk(src_dir):
        # Skip out_dir subtree
        # Normalize both for comparison
        root_path = Path(root)
        # Prune "out" subtree early
        dirs[:] = [d for d in dirs if (root_path / d) != out_dir]

        for fname in files:
            p = root_path / fname
            if not is_lua_file(p):
                continue
            rel = p.relative_to(src_dir)
            if rel.parts and rel.parts[0] == out_dir.name:
                continue

            stem = p.stem  # filename without .lua
            # dotted path alias
            dotted = ".".join(rel.with_suffix("").parts)
            # Also add segmented path alias that Lua commonly resolves (dots -> slashes)
            # For nested files, the full dotted path is the canonical key.
            register(dotted, p)
            # Also allow plain stem (last segment) as a fallback key if unique
            register(stem, p)

    return mod_by_require, aliases

# Resolve a require-string to Path, trying dotted-path → filesystem path
def resolve_require_to_path(req: str, src_dir: Path, mod_index: Dict[str, Path]) -> Optional[Path]:
    # Exact dotted match first
    if req in mod_index:
        return mod_index[req]

    # Try dotted -> filesystem path a/b/c.lua
    dotted_path = src_dir / Path(req.replace(".", "/") + ".lua")
    if dotted_path.exists():
        return dotted_path

    # Try plain name fallback already covered by mod_index on stem registration
    return None

# Process one Lua file into bundled output text
def bundle_one_file(
    entry_path: Path,
    src_dir: Path,
    mod_index: Dict[str, Path],
) -> str:
    entry_text = read_text(entry_path)

    # Track insertion order and processed module texts
    processed: Dict[str, str] = {}            # req-string -> (require-replaced module content)
    order: List[str] = []                     # insertion order (deps before dependents)
    varname_for: Dict[str, str] = {}          # req-string -> variable name used in replacement
    seen_stack: Set[str] = set()              # guard cycles during DFS

    # DFS process a module by its require-string
    def dfs_require(req: str):
        if req in processed or req in seen_stack:
            return
        path = resolve_require_to_path(req, src_dir, mod_index)
        if not path:
            return  # Unknown / external module; do not inline
        seen_stack.add(req)

        text = read_text(path)
        # First find its child requires
        child_reqs = find_requires(text)
        for child in child_reqs:
            # Only follow if we can inline (exists in index)
            if resolve_require_to_path(child, src_dir, mod_index):
                dfs_require(child)

        # After children are handled, replace requires inside this module
        # with their variable names (if we will inline them)
        # Prepare known map for replacement
        local_known = {}
        for child in child_reqs:
            if resolve_require_to_path(child, src_dir, mod_index):
                local_known[child] = varname_for.setdefault(child, sanitize_var(child))
        new_text = replace_requires(text, local_known)

        # Assign varname for this module itself
        varname_for.setdefault(req, sanitize_var(req))
        processed[req] = new_text
        order.append(req)
        seen_stack.remove(req)

    # Kick off from the entry file's immediate requires
    top_reqs = find_requires(entry_text)
    for r in top_reqs:
        if resolve_require_to_path(r, src_dir, mod_index):
            dfs_require(r)

    # Now, build header inserts in dependency order
    headers: List[str] = []
    for req in order:
        var = varname_for[req]
        # Prefer displaying the actual filename if available
        p = resolve_require_to_path(req, src_dir, mod_index)
        shown = f"{Path(req).name}.lua"
        if p is not None:
            shown = p.name
        wrapped = (
            f"-- {shown}\n"
            f"local {var} = (function ()\n"
            f"{indent_block(processed[req], 4)}"
            f"end)()\n"
        )
        headers.append(wrapped)

    # Replace requires in the entry file
    entry_known: Dict[str, str] = {}
    for r in top_reqs:
        if resolve_require_to_path(r, src_dir, mod_index):
            entry_known[r] = varname_for.setdefault(r, sanitize_var(r))
    entry_new = replace_requires(entry_text, entry_known)

    # Compose final
    final = ""
    if headers:
        final += "".join(headers) + "\n"
    final += f"-- {entry_path.name}\n" + entry_new
    return final

def main():
    parser = argparse.ArgumentParser(description="Inline Lua requires into each .lua file.")
    parser.add_argument("src_dir", nargs="?", default=".", help="源目录（可选，默认当前目录）")
    parser.add_argument("out_dir", nargs="?", default=None, help="输出目录（可选，默认 <src_dir>/out）")
    parser.add_argument("--minify", action="store_true", help="移除缩进/回车与注释，做最小化输出")
    args = parser.parse_args()

    src_dir = Path(args.src_dir).resolve()
    out_dir = Path(args.out_dir).resolve() if args.out_dir else (src_dir / "out").resolve()

    if not src_dir.exists():
        print(f"Source directory not found: {src_dir}", file=sys.stderr)
        sys.exit(1)

    mod_index, _aliases = build_module_index(src_dir, out_dir)

    to_process: List[Path] = []
    for root, dirs, files in os.walk(src_dir):
        root_path = Path(root)
        dirs[:] = [d for d in dirs if (root_path / d).resolve() != out_dir]
        for fname in files:
            p = root_path / fname
            if not is_lua_file(p):
                continue
            if out_dir in p.resolve().parents or p.parent.resolve() == out_dir:
                continue
            to_process.append(p)

    for p in to_process:
        bundled = bundle_one_file(p, src_dir, mod_index)
        if args.minify:
            bundled = lua_minify(bundled)
        rel = p.relative_to(src_dir)
        out_path = out_dir / rel
        write_text(out_path, bundled)

if __name__ == "__main__":
    main()