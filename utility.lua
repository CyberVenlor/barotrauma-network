
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