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

-- switch.lua
local mac = mac
local data_link = data_link_layer
local core = core
local util = utility

local mac_table = {}

local function broadcast(data, exception)
    for i = 1, 30 do
        if i ~= exception then
            core.tx(i, data)
        end
    end
end

function inp(pin, val)
    local bytes = util.string_to_bytes(val)
    local frame = data_link.bytes_to_frame(bytes)
    if frame == nil then
        return
    end
    local src_mac = mac.mac_to_string(frame.src_mac)
    -- learn new mac
    mac_table[src_mac] = {pin = pin, time = core.time()}
    --print("src_mac: " .. src_mac .. ", dst_mac: ".. mac.mac_to_string(frame.dst_mac) .. ", pin: " .. pin .. ", time: ", core.time())
    --print(serde.serialize(mac_table))
    -- broadcast
    if mac.is_broadcast_mac(frame.dst_mac) or mac.is_multicast_mac(frame.dst_mac) then
        --print("broadcast")
        broadcast(val, pin)
        return
    end
    local dst_mac = mac.mac_to_string(frame.dst_mac)
    -- 泛洪
    local pair = mac_table[dst_mac]
    if pair == nil then
        --print("泛洪")
        broadcast(val, pin)
        return
    end
    -- 正常播送
    core.tx(pair.pin, val)
end

function upd()
    -- 老化
    for key, value in pairs(mac_table) do
        if core.time() - value.time > 60 then
            mac_table[key] = nil
        end
    end
end