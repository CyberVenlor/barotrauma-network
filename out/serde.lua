-- serde.lua
-- serde.lua  (Lua 5.2)
-- serialize/deserialize Lua tables to human-readable Lua-like literals.
-- No load(), no third-party libs.

local M = {}

local DEFAULTS = {
  indent = "  ",     -- two spaces
  sort_keys = true,  -- deterministic output
}

-- ===== utilities =====
local function is_identifier(s)
  return type(s) == "string" and s:match("^[A-Za-z_][A-Za-z0-9_]*$") ~= nil
end

local function escape_str(s)
  local map = {
    ["\\"] = "\\\\", ["\n"] = "\\n", ["\r"] = "\\r", ["\t"] = "\\t",
    ['"']  = '\\"',  ["\a"] = "\\a", ["\b"] = "\\b", ["\f"] = "\\f", ["\v"] = "\\v",
  }
  return '"' .. s:gsub('[%z\1-\31\\"]', function(c)
    return map[c] or string.format("\\x%02X", string.byte(c))
  end) .. '"'
end

local function is_finite(n)
  return n == n and n ~= math.huge and n ~= -math.huge
end

local function seq_len(t)
  local i = 1
  while rawget(t, i) ~= nil do i = i + 1 end
  return i - 1
end

local function sort_keys(keys)
  local order = { number = 1, string = 2, boolean = 3, ["nil"] = 4 }
  table.sort(keys, function(a, b)
    local ta, tb = type(a), type(b)
    if ta ~= tb then return (order[ta] or 9) < (order[tb] or 9) end
    if ta == "string" then return a < b end
    if ta == "number" then return a < b end
    if ta == "boolean" then return (a and 1 or 0) < (b and 1 or 0) end
    return tostring(a) < tostring(b)
  end)
end

local function all_scalar_array(t, n)
  for i = 1, n do
    local tv = type(t[i])
    if tv == "table" or tv == "function" or tv == "userdata" or tv == "thread" then
      return false
    end
  end
  return true
end

-- ===== serialize =====
local function serialize_value(v, level, opts, seen)
  local tv = type(v)
  if tv == "nil" then
    return "nil"
  elseif tv == "boolean" then
    return v and "true" or "false"
  elseif tv == "number" then
    if not is_finite(v) then error("cannot serialize NaN or infinity") end
    return string.format("%.17g", v)
  elseif tv == "string" then
    return escape_str(v)
  elseif tv == "table" then
    if seen[v] then error("circular reference detected") end
    seen[v] = true

    local nseq = seq_len(v)
    local keys = {}
    for k, _ in pairs(v) do
      if not (type(k) == "number" and k >= 1 and k <= nseq and k % 1 == 0) then
        keys[#keys + 1] = k
      end
    end
    if opts.sort_keys then sort_keys(keys) end

    -- inline pure scalar arrays: {1, 2, 3}
    if nseq > 0 and #keys == 0 and all_scalar_array(v, nseq) then
      local arr = {}
      for i = 1, nseq do
        arr[i] = serialize_value(v[i], level + 1, opts, seen)
      end
      seen[v] = nil
      return "{" .. table.concat(arr, ", ") .. "}"
    end

    -- pretty multi-line
    local indent_cur = string.rep(opts.indent, level)
    local indent_in  = indent_cur .. opts.indent
    local parts = {}

    for i = 1, nseq do
      parts[#parts + 1] = serialize_value(v[i], level + 1, opts, seen)
    end
    for _, k in ipairs(keys) do
      local kt = type(k)
      local keystr
      if is_identifier(k) then
        keystr = k .. " = "
      elseif kt == "string" or kt == "number" or kt == "boolean" then
        local krep = (kt == "string") and escape_str(k)
          or (kt == "number") and string.format("%.17g", k)
          or (k and "true" or "false")
        keystr = "[" .. krep .. "] = "
      else
        error("unsupported key type: " .. kt)
      end
      local val = serialize_value(v[k], level + 1, opts, seen)
      parts[#parts + 1] = keystr .. val
    end

    seen[v] = nil
    if #parts == 0 then return "{}" end
    return "{\n" .. indent_in .. table.concat(parts, ",\n" .. indent_in) .. "\n" .. indent_cur .. "}"
  else
    error("unsupported type: " .. tv)
  end
end

function M.serialize(value, opts)
  opts = opts or {}
  for k, v in pairs(DEFAULTS) do if opts[k] == nil then opts[k] = v end end
  return serialize_value(value, 0, opts, {})
end

-- ===== deserialize (tiny recursive-descent parser for our own format) =====
local Lexer = {}
Lexer.__index = Lexer

local function is_space(c) return c == " " or c == "\t" or c == "\n" or c == "\r" end
local function is_alpha(c) return c and c:match("[A-Za-z_]") ~= nil end
local function is_alnum(c) return c and c:match("[A-Za-z0-9_]") ~= nil end
local function is_digit(c) return c and c:match("%d") ~= nil end
local function is_hex(c) return c and c:match("[%da-fA-F]") ~= nil end

function Lexer.new(s)
  return setmetatable({ s = s, i = 1, n = #s, tok = nil, val = nil }, Lexer)
end

function Lexer:peek()
  return (self.i <= self.n) and self.s:sub(self.i, self.i) or nil
end

function Lexer:nextc()
  local c = self:peek()
  self.i = self.i + 1
  return c
end

function Lexer:skip_ws()
  while true do
    local c = self:peek()
    if c and is_space(c) then
      self:nextc()
    else
      break
    end
  end
end

function Lexer:read_string()
  local quote = self:nextc() -- must be "
  local buf = {}
  while true do
    local c = self:nextc()
    if not c then error("unterminated string") end
    if c == '"' then break end
    if c == "\\" then
      local e = self:nextc()
      if not e then error("unterminated escape") end
      if e == "n" then buf[#buf+1] = "\n"
      elseif e == "r" then buf[#buf+1] = "\r"
      elseif e == "t" then buf[#buf+1] = "\t"
      elseif e == "a" then buf[#buf+1] = "\a"
      elseif e == "b" then buf[#buf+1] = "\b"
      elseif e == "f" then buf[#buf+1] = "\f"
      elseif e == "v" then buf[#buf+1] = "\v"
      elseif e == "\\" then buf[#buf+1] = "\\"
      elseif e == '"'  then buf[#buf+1] = '"'
      elseif e == "x" then
        local h1, h2 = self:nextc(), self:nextc()
        if not (is_hex(h1) and is_hex(h2)) then error("bad \\x escape") end
        buf[#buf+1] = string.char(tonumber(h1 .. h2, 16))
      elseif e:match("%d") then
        -- decimal escape \ddd (up to 3 digits)
        local d2 = self:peek()
        local d3
        local esc = e
        if d2 and d2:match("%d") then
          esc = esc .. self:nextc()
          d3 = self:peek()
          if d3 and d3:match("%d") then esc = esc .. self:nextc() end
        end
        buf[#buf+1] = string.char(tonumber(esc, 10))
      else
        error("unknown escape \\" .. e)
      end
    else
      buf[#buf+1] = c
    end
  end
  return table.concat(buf)
end

function Lexer:read_number_or_ident()
  local start = self.i
  local c = self:peek()
  if c == '-' then self:nextc(); c = self:peek() end
  if is_digit(c) then
    -- number
    while is_digit(self:peek()) do self:nextc() end
    if self:peek() == '.' then
      self:nextc()
      while is_digit(self:peek()) do self:nextc() end
    end
    local e = self:peek()
    if e == 'e' or e == 'E' then
      self:nextc()
      local sgn = self:peek()
      if sgn == '+' or sgn == '-' then self:nextc() end
      if not is_digit(self:peek()) then error("bad exponent") end
      while is_digit(self:peek()) do self:nextc() end
    end
    local str = self.s:sub(start, self.i - 1)
    local num = tonumber(str)
    if not num then error("bad number: " .. str) end
    self.tok, self.val = "number", num
    return
  end
  -- identifier
  self.i = start
  if is_alpha(self:peek()) then
    local buf = {}
    buf[#buf+1] = self:nextc()
    while is_alnum(self:peek()) do buf[#buf+1] = self:nextc() end
    local id = table.concat(buf)
    if id == "true" or id == "false" then
      self.tok, self.val = "boolean", (id == "true")
    elseif id == "nil" then
      self.tok, self.val = "nil", nil
    else
      self.tok, self.val = "ident", id
    end
    return
  end
  error("unexpected char at " .. self.i .. ": " .. tostring(self:peek()))
end

function Lexer:next()
  self:skip_ws()
  local c = self:peek()
  if not c then self.tok, self.val = "eof", nil; return end
  if c == '{' or c == '}' or c == '[' or c == ']' or c == '=' or c == ',' then
    self.tok, self.val = c, c; self:nextc(); return
  end
  if c == '"' then
    self.tok, self.val = "string", self:read_string(); return
  end
  self:read_number_or_ident()
end

-- Parser
local Parser = {}
Parser.__index = Parser

function Parser.new(s)
  local lx = Lexer.new(s)
  lx:next()
  return setmetatable({ lx = lx }, Parser)
end

function Parser:expect(tok)
  if self.lx.tok ~= tok then
    error("expected '" .. tok .. "', got '" .. tostring(self.lx.tok) .. "'")
  end
  local v = self.lx.val
  self.lx:next()
  return v
end

function Parser:parse_value()
  local t = self.lx.tok
  if t == "string" or t == "number" then
    local v = self.lx.val; self.lx:next(); return v
  elseif t == "boolean" then
    local v = self.lx.val; self.lx:next(); return v
  elseif t == "nil" then
    self.lx:next(); return nil
  elseif t == "{" then
    return self:parse_table()
  else
    error("unexpected token in value: " .. tostring(t))
  end
end

function Parser:parse_bracket_key()
  self:expect("[")
  local t = self.lx.tok
  local key
  if t == "string" or t == "number" or t == "boolean" then
    key = self.lx.val; self.lx:next()
  else
    error("only string/number/boolean keys supported inside []")
  end
  self:expect("]")
  return key
end

function Parser:parse_table()
  self:expect("{")
  local res, next_index = {}, 1
  while self.lx.tok ~= "}" do
    local t = self.lx.tok
    if t == "ident" then
      local name = self.lx.val; self.lx:next()
      self:expect("=")
      res[name] = self:parse_value()
    elseif t == "[" then
      local key = self:parse_bracket_key()
      self:expect("=")
      res[key] = self:parse_value()
    else
      -- array style element
      res[next_index] = self:parse_value()
      next_index = next_index + 1
    end
    if self.lx.tok == "," then
      self.lx:next()
      -- allow trailing comma before }
      if self.lx.tok == "}" then break end
    end
  end
  self:expect("}")
  return res
end

function M.deserialize(s)
  local p = Parser.new(s)
  local v = p:parse_value()
  if p.lx.tok ~= "eof" then error("trailing content") end
  return v
end

return M