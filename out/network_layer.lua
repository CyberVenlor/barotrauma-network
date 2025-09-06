-- serde.lua
local serde = (function ()
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
end)()
-- core.lua
local core = (function ()
    local M = {}

    function M.tx(pin, data)
    ---@diagnostic disable-next-line: undefined-global
        out[pin] = data
    end

    function M.time()
    ---@diagnostic disable-next-line: undefined-global
        return time()
    end

    -- function inp(pin, val)
    
    -- end

    -- function upd(deltaTime)
    
    -- end

    return M
end)()
-- utility.lua
local utility = (function ()

    local M = {}

    function M.bytes_to_string(bs)
        if type(bs) ~= "table" then error("bytes_to_string: need table") end
        local t, n = {}, #bs
        for i = 1, n do
            local v = bs[i]
            if type(v) ~= "number" then error("bytes_to_string: non-number at "..i) end
            if v ~= math.floor(v) then error("bytes_to_string: non-integer at "..i) end
            if v < 0 or v > 255 then error("bytes_to_string: out of range at "..i) end
            t[i] = string.char(v)
        end
        return table.concat(t)
    end
  
      -- string -> table(byte[])
    function M.string_to_bytes(s)
        if type(s) ~= "string" then error("string_to_bytes: need string") end
        local n = #s
        local o = {}
        for i = 1, n do
            o[i] = string.byte(s, i)
        end
        return o
    end
  
      -- 工具：拼接
    function M.concat_bytes(a,b)
        local o = {}
        for i=1,#a do o[#o+1]=a[i] end
        for i=1,#b do o[#o+1]=b[i] end
        return o
    end

    function M.bytes_equal(a, b)
        if type(a) ~= "table" or type(b) ~= "table" then
            return false, "both arguments must be tables"
        end
        if #a ~= #b then return false end
        for i = 1, #a do
            local x, y = a[i], b[i]
            if type(x) ~= "number" or type(y) ~= "number" then
                return false, "non-number element at index " .. i
            end
            if x ~= y then return false end
        end
        return true
    end

    function M.assert_byte(n, label)
        assert(type(n) == "number" and n >= 0 and n <= 255 and n == math.floor(n),
            (label or "byte") .. " must be integer 0..255")
    end
  
      -- 工具：u16
    function M.u16_to_bytes(n)
        return { math.floor(n/256)%256 , n%256 }
    end
    function M.bytes_to_u16(b1,b2)
        return b1*256 + b2
    end

    local hex_map = {
        ["0"]=0, ["1"]=1, ["2"]=2, ["3"]=3, ["4"]=4, ["5"]=5,
        ["6"]=6, ["7"]=7, ["8"]=8, ["9"]=9,
        ["A"]=10, ["B"]=11, ["C"]=12, ["D"]=13, ["E"]=14, ["F"]=15,
        ["a"]=10, ["b"]=11, ["c"]=12, ["d"]=13, ["e"]=14, ["f"]=15,
    }

    function M.int_to_bytes(val, size)
        local out = {}
        for i=size-1,0,-1 do
            out[#out+1] = math.floor(val / 256^i) % 256
        end
        return out
    end

    -- 从字节数组拼成整数（大端）
    function M.bytes_to_int(bytes, offset, size)
        local v = 0
        for i=0,size-1 do
            v = v * 256 + bytes[offset+i]
        end
        return v
    end

    function M.hex_byte(a, b)
        local hi, lo = hex_map[a], hex_map[b]
        if not hi or not lo then
            error("bad hex at position")
        end
        return hi * 16 + lo
    end

    return M
end)()
-- ip.lua
local ip = (function ()
    local M = {}

    local util = utility

    -- "192.168.0.1" -> {192,168,0,1}
    function M.string_to_ipv4(str)
        assert(type(str) == "string", "input must be string")
        local parts = {}
        for num in str:gmatch("(%d+)") do
            local n = tonumber(num)
            assert(n and n >= 0 and n <= 255, "invalid IPv4 segment: " .. tostring(num))
            parts[#parts+1] = n
        end
        assert(#parts == 4, "IPv4 must have 4 segments")
        return parts
    end

    -- {192,168,0,1} -> "192.168.0.1"
    function M.ipv4_to_string(ip)
        assert(type(ip) == "table", "input must be table")
        assert(#ip == 4, "IPv4 table must have 4 elements")
        for i=1,4 do
            local b = ip[i]
            assert(type(b) == "number" and b >= 0 and b <= 255, "invalid IPv4 byte at "..i)
        end
        return table.concat(ip, ".")
    end

    function M.ip_to_u32(ip)  -- {a,b,c,d} -> u32
        return util.bytes_to_int(ip)
      end
    function M.u32_to_ip(u)
        return util.int_to_bytes(u, 4)
    end

    function M.check_ip(ip)
        assert(type(ip) == "table", "ip must be table")
        assert(#ip == 4, "ip must have 4 bytes")
        for i=1,4 do
            local b = ip[i]
            assert(type(b) == "number" and b >= 0 and b <= 255, "ip ["..i.."] invalid")
        end
    end

    return M
end)()
-- mac.lua
local mac = (function ()
    local M = {}

      -- 广播 MAC
    M.BROADCAST = {0xFF,0xFF,0xFF,0xFF,0xFF,0xFF}

    function M.gen_mac()
        local mac = {}
        -- 确保第一个字节的最低位为0（即单播地址）
        mac[1] = math.random(0, 127)  -- 第一个字节范围是 0 到 127，保证最低位为0
        for i = 2, 6 do
             mac[i] = math.random(0, 255)
        end
        return mac
    end
      -- 转换字节数组为字符串
    function M.mac_to_string(mac)
        return string.format("%02X:%02X:%02X:%02X:%02X:%02X",
            mac[1], mac[2], mac[3], mac[4], mac[5], mac[6])
    end
  
    function M.string_to_mac(s)
        local mac = {}
            -- 用正则抓取两个十六进制
        for hi, lo in s:gmatch("(%x)(%x)") do
            table.insert(mac, utility.hex_byte(hi, lo))
        end
        if #mac ~= 6 then
            error("invalid MAC format")
        end
        return mac
    end

    function M.check_mac(mac)
        assert(type(mac) == "table", "MAC must be a table")
        assert(#mac == 6, "MAC must have 6 bytes")
        for i = 1, 6 do
            local b = mac[i]
            assert(type(b) == "number" and b >= 0 and b <= 255, "MAC [" .. i .. "] invalid byte")
        end
    end
  
    function M.is_broadcast_mac(m) return utility.bytes_equal(m, M.BROADCAST) end
    -- function M.is_multicast_mac(m) return (m[1] % 2) == 1 end -- 低位bit=1
    function M.is_multicast_mac(m) return false end -- 低位bit=1

    return M
end)()
-- physics_layer.lua
local physics_layer = (function ()
    local M = {}

    local util = utility

    function M.tx(bytes)
        local s = util.bytes_to_string(bytes)
        core.tx(10, s)
    end

    function M.rx(string)
        local b = util.string_to_bytes(string)
        return b
    end

    return M
end)()
-- data_link_layer.lua
local data_link_layer = (function ()
    local M = {}
    local util = utility
    local mac = mac
    local physics = physics_layer

    M.MAC = mac.gen_mac()

    M.ETHERTYPE = {
        IPv4 = 0x0800,
        IPv6 = 0x86DD,
        ARP  = 0x0806,
        LL   = 0x88B5,
    }
  
    -- Ethernet II frame <-> bytes

    function M.frame_to_bytes(f)
        assert(type(f) == "table", "f must be a table")

        mac.check_mac(f.dst_mac)
        mac.check_mac(f.src_mac)

        assert(type(f.ethertype) == "number" and f.ethertype >= 0 and f.ethertype <= 0xFFFF
               and f.ethertype == math.floor(f.ethertype),
               "f.ethertype must be integer 0..65535")
        assert(type(f.payload) == "table", "f.payload must be a table")

        local bytes = {}

        -- dst_mac (6) + src_mac (6)
        for i = 1, 6 do util.assert_byte(f.dst_mac[i], "dst_mac["..i.."]"); bytes[#bytes+1] = f.dst_mac[i] end
        for i = 1, 6 do util.assert_byte(f.src_mac[i], "src_mac["..i.."]"); bytes[#bytes+1] = f.src_mac[i] end

        -- EtherType (big-endian 2 bytes) without bit ops
        local hi = math.floor(f.ethertype / 256)
        local lo = f.ethertype % 256
        bytes[#bytes+1] = hi
        bytes[#bytes+1] = lo

        -- payload
        for i = 1, #f.payload do
            local b = f.payload[i]
            util.assert_byte(b, "payload["..i.."]")
            bytes[#bytes+1] = b
        end

        return bytes
    end

    function M.bytes_to_frame(bytes)
        assert(type(bytes) == "table", "bytes must be a table")
        assert(#bytes >= 14, "not enough bytes for Ethernet II header (need >=14)")

        for i = 1, #bytes do util.assert_byte(bytes[i], "bytes["..i.."]") end

        local f = {}
        f.dst_mac = { bytes[1], bytes[2], bytes[3], bytes[4], bytes[5], bytes[6] }
        f.src_mac = { bytes[7], bytes[8], bytes[9], bytes[10], bytes[11], bytes[12] }

        -- EtherType (big-endian) without bit ops
        f.ethertype = bytes[13] * 256 + bytes[14]

        f.payload = {}
        for i = 15, #bytes do
            f.payload[#f.payload+1] = bytes[i]
        end

        return f
    end

    function M.abort_receive(src_mac, dst_mac)
        -- 目的过滤：只收自己/广播/组播
        if not (utility.bytes_equal(dst_mac, src_mac) or M.is_broadcast_mac(dst_mac) or M.is_multicast_mac(dst_mac) ) then
            return true
        end
    
        return false
    end

    function M.tx(dst_mac, payload, ethertype)
        ethertype = ethertype or M.ETHERTYPE.LL

        local frame = {
            dst_mac = dst_mac,
            src_mac = M.MAC,
            ethertype = ethertype,
            payload = payload
        }

        local bytes = M.frame_to_bytes(frame)
        physics.tx(bytes)
    end

    function M.rx(bytes)
        local frame = M.bytes_to_frame(bytes)
        if not (util.bytes_equal(M.MAC, frame.dst_mac) or mac.is_broadcast_mac(frame.dst_mac) or mac.is_multicast_mac(frame.dst_mac) ) then
            return
        end
        return {
            payload = frame.payload,
            src_mac = frame.src_mac,
            ethertype = frame.ethertype
        }
    end

    local f = {
        dst_mac = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF},
        src_mac = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF},
        ethertype = M.ETHERTYPE.IPv4,
        payload = {0xDD}
    }

    return M
end)()

-- network_layer.lua
local M = {}
local serde = serde
local core = core
local ip = ip
local mac_mod = mac
local util = utility
local data_link = data_link_layer
-- ipv4 protocol
-- 版本
-- 首部长度4b
-- ttl 8b
-- 协议 8b
-- src_ip 32b
-- dst_ip 32b
-- payload

M.PROTO_TYPE = {
    ICMP = 1,
    TCP  = 6,
    UDP  = 17,
}

M.IP = {192, 168, 1, 1}

M.iface = function ()
    return {
        ip = M.IP,
        mask = 24,
        mac = data_link.MAC
    }
end
M.gateway = {192, 168, 1, 1}
M.arp_ttl = 60                -- 秒
local arp_cache = {}
local pend = {}

local function same_subnet(a, b, mask)
    local full = math.floor(mask / 8)
    local rem  = mask - full * 8
    for i = 1, full do
      if a[i] ~= b[i] then return false end
    end
    if rem > 0 then
      local step = 2^(8 - rem)                -- 每块大小
      if math.floor(a[full+1] / step) ~= math.floor(b[full+1] / step) then
        return false
      end
    end
    return true
end

local arp_pkt = {
    htype      = 1,   -- Ethernet
    ptype      = 0x0800, -- IPv4
    hlen       = 6,   -- MAC 长度
    plen       = 4,   -- IPv4 长度
    opcode     = 1,   -- 1=Request, 2=Reply
  
    sender_mac = {0xAA,0xBB,0xCC,0xDD,0xEE,0xFF},  -- 本机 MAC
    sender_ip  = {192,168,1,2},                    -- 本机 IP
    target_mac = {0,0,0,0,0,0},                    -- 请求时置 0
    target_ip  = {192,168,1,1},                    -- 想解析的 next_hop IP
}

local function arp_lookup(ip_tbl)
    local ent = arp_cache[ip.ipv4_to_string(ip_tbl)]
    if not ent then return nil end
    if ent.expire_at and ent.expire_at <= core.time() then return nil end
    return ent.mac
end

local function arp_learn(sender_ip_tbl, sender_mac_tbl)
    arp_cache[ip.ipv4_to_string(sender_ip_tbl)] = { mac = sender_mac_tbl, expire_at = core.time() + M.arp_ttl }
    print(serde.serialize(arp_cache))
end
  

local function has_pending_nh(nh)
    for i = 1, #pend do
      if util.bytes_equal(pend[i].next_hop, nh) then
        return true
      end
    end
    return false
  end
  
  -- 学到 ARP 后冲队，只处理 next_hop==sender_ip 的项
local function resend_pending(sender_ip)
    local i = 1
    while i <= #pend do
      local it = pend[i]
      if util.bytes_equal(it.next_hop, sender_ip) then
        table.remove(pend, i)          -- 先移除再发，避免再次MISS时重复入队
        M.tx(it.dst_ip, it.payload, it.proto)
      else
        i = i + 1
      end
    end
end

function M.package_to_bytes(pkt)
    assert(type(pkt) == "table", "pkt must be a table")
    assert(type(pkt.version) == "number", "pkt.version must be number")
    assert(type(pkt.ihl) == "number", "pkt.ihl must be number")
    assert(type(pkt.ttl) == "number", "pkt.ttl must be number")
    assert(type(pkt.proto) == "number", "pkt.proto must be number")

    ip.check_ip(pkt.src_ip)
    ip.check_ip(pkt.dst_ip)
    assert(type(pkt.payload) == "table", "pkt.payload must be table")

    -- build first byte: version(4b) | IHL(4b)
    local first = pkt.version * 16 + (pkt.ihl % 16)
    util.assert_byte(first, "first")

    local bytes = {}
    bytes[#bytes+1] = first
    bytes[#bytes+1] = math.floor(pkt.ttl % 256)
    bytes[#bytes+1] = math.floor(pkt.proto % 256)

    for i=1,4 do util.assert_byte(pkt.src_ip[i], "src_ip["..i.."]"); bytes[#bytes+1] = pkt.src_ip[i] end
    for i=1,4 do util.assert_byte(pkt.dst_ip[i], "dst_ip["..i.."]"); bytes[#bytes+1] = pkt.dst_ip[i] end

    for i=1,#pkt.payload do
        local b = pkt.payload[i]
        util.assert_byte(b, "payload["..i.."]")
        bytes[#bytes+1] = b
    end

    return bytes
end

function M.bytes_to_package(bytes)
    assert(type(bytes) == "table", "bytes must be table")
    assert(#bytes >= 11, "not enough bytes for ipv4 header (need >=11)")

    for i=1,#bytes do util.assert_byte(bytes[i], "bytes["..i.."]") end

    local pkt = {}
    local first = bytes[1]
    pkt.version = math.floor(first / 16)
    pkt.ihl     = first % 16
    pkt.ttl     = bytes[2]
    pkt.proto   = bytes[3]

    pkt.src_ip = { bytes[4], bytes[5], bytes[6], bytes[7] }
    pkt.dst_ip = { bytes[8], bytes[9], bytes[10], bytes[11] }

    pkt.payload = {}
    for i=12,#bytes do
        pkt.payload[#pkt.payload+1] = bytes[i]
    end

    return pkt
end

local pkt = {
    version = 4,
    ihl = 5,
    ttl = 64,
    proto = 6, -- TCP
    src_ip = {192, 168, 1, 1}, -- 192.168.1.1
    dst_ip = {192, 168, 1, 1}, -- 8.8.8.8
    payload = {1,2,3,4}
}

function M.tx(dst_ip, payload, proto)
    proto = proto or M.PROTO_TYPE.UDP
    print("dst_ip =" .. serde.serialize(dst_ip))
    local ifc = M.iface()
    print("iface =" .. serde.serialize(ifc))
    if not ifc or not ifc.ip or not ifc.mask then return "NO_IFACE" end

    local next_hop
    if same_subnet(dst_ip, ifc.ip, ifc.mask) then
      next_hop = dst_ip               -- 直连
    elseif M.gateway then
      next_hop = M.gateway         -- 走默认网关
    else
      return "NO_ROUTE"
    end
    print("next_hop=" .. serde.serialize(next_hop))

    local mac = arp_lookup(next_hop)
    if not mac then
        -- 这里可顺手触发一次 ARP 请求，但不等待；本次直接失败
        local arp_pkt = {
            htype      = 1,   -- Ethernet
            ptype      = 0x0800, -- IPv4
            hlen       = 6,   -- MAC 长度
            plen       = 4,   -- IPv4 长度
            opcode     = 1,   -- 1=Request, 2=Reply
        
            sender_mac = ifc.mac,  -- 本机 MAC
            sender_ip  = ifc.ip,           -- 本机 IP
            target_mac = {0,0,0,0,0,0},                    -- 请求时置 0
            target_ip  = dst_ip                   -- 想解析的 next_hop IP
        }
        local has_pending = has_pending_nh(next_hop)
        table.insert(pend, {
            next_hop = next_hop,
            dst_ip = dst_ip,
            payload = payload,
            proto = proto,
        })
        print("pendd:" .. serde.serialize(pend))
        if not has_pending then
            data_link.tx(mac_mod.BROADCAST, util.string_to_bytes(serde.serialize(arp_pkt)), data_link.ETHERTYPE.ARP)
        end
        
        return
    end

    print("mac=", serde.serialize(mac))

    local pkt = {
      version = 4, ihl = 5, ttl = 64, proto = proto,
      src_ip = ifc.ip, dst_ip = dst_ip, payload = payload
    }

    data_link.tx(mac, util.string_to_bytes(serde.serialize(pkt)), data_link.ETHERTYPE.IPv4)

    return "SUCCESS"
end

function M.rx(frame)
    if frame == nil then return end
    local ifc = M.iface()
    if frame.ethertype == data_link.ETHERTYPE.ARP then
        local receive_pkt = serde.deserialize(util.bytes_to_string(frame.payload))
        if receive_pkt == nil then return end
        if not util.bytes_equal(receive_pkt.target_ip, ifc.ip) then return end
        if receive_pkt.opcode == 1 then
            local send_pkt = {
                htype      = 1,   -- Ethernet
                ptype      = 0x0800, -- IPv4
                hlen       = 6,   -- MAC 长度
                plen       = 4,   -- IPv4 长度
                opcode     = 2,   -- 1=Request, 2=Reply
            
                sender_mac = ifc.mac,  -- 本机 MAC
                sender_ip  = ifc.ip,           -- 本机 IP
                target_mac = receive_pkt.sender_mac,             -- 请求时置 0
                target_ip  = receive_pkt.sender_ip            -- 想解析的 next_hop IP
            }
            print("sendpkg:" .. serde.serialize(send_pkt))
            data_link.tx(receive_pkt.sender_mac, util.string_to_bytes(serde.serialize(send_pkt)), data_link.ETHERTYPE.ARP)
        elseif receive_pkt.opcode == 2 then
            arp_learn(receive_pkt.sender_ip, receive_pkt.sender_mac)
            print("pend:"..serde.serialize(pend))
            resend_pending(receive_pkt.sender_ip)
        end
    elseif frame.ethertype == data_link.ETHERTYPE.IPv4 then
        local receive_pkt = serde.deserialize(util.bytes_to_string(frame.payload))
        if receive_pkt == nil then return end
        if receive_pkt.version ~= 4 then return end
        if not util.bytes_equal(receive_pkt.dst_ip, ifc.ip) then return end
        return receive_pkt.payload
    end
end

return M