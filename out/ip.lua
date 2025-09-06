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