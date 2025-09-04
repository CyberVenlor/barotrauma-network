local M = {}
local ip = require("ip")
local util = require("utility")
-- ipv4 protocol
-- 版本
-- 首部长度4b
-- ttl 8b
-- 协议 8b
-- src_ip 32b
-- dst_ip 32b
-- payload

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

return M