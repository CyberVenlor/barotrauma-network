local M = {}

local util = require("utility")

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