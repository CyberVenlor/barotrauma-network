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

    function M.hex_byte(a, b)
        local hi, lo = hex_map[a], hex_map[b]
        if not hi or not lo then
            error("bad hex at position")
        end
        return hi * 16 + lo
    end

    return M
end)()

-- data_link.lua
local M = {}
local util = utility

M.ETHERTYPE = {
    IPv4 = 0x0800,
    ARP  = 0x0806,
    LL   = 0x88B5,
}
  
function M.transmit_data(src_mac, dst_mac, payload, etype)
    if (type(src_mac) ~= "table" or type(dst_mac) ~= "table" or type(payload) ~= "table") then 
        error("funtion transmit_data receive error type value")
    end
    etype = etype or M.ETHERTYPE.LL
    -- 头部
    local frame = {}
    for i=1,6 do frame[#frame+1] = dst_mac[i] end
    for i=1,6 do frame[#frame+1] = src_mac[i] end
    local etb = util.u16_to_bytes(etype)
    frame[#frame+1] = etb[1]; frame[#frame+1] = etb[2]

    -- 载荷
    frame = util.concat_bytes(frame, payload)
    
    local wire = util.bytes_to_string(frame)
    -- 发到导线
    return wire
end
  
  -- 接收
  
function M.receive_data(data) -- data: string (bytes)
  
    local bytes = util.string_to_bytes(data)
    if #bytes < 14 then return end
  
    local dst = {bytes[1],bytes[2],bytes[3],bytes[4],bytes[5],bytes[6]}
    local src = {bytes[7],bytes[8],bytes[9],bytes[10],bytes[11],bytes[12]}
    local et  = util.bytes_to_u16(bytes[13],bytes[14])
    local payload = {}
    for i=15,#bytes do payload[#payload+1] = bytes[i] end
  
    return {
        dst = dst,
        src = src,
        ethertype = et,
        payload = payload,
    }
end

return M