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
