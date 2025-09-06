local M = {}
local util = require("utility")
local mac = require("mac")
local physics = require("physics_layer")

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
    if not (require("utility").bytes_equal(dst_mac, src_mac) or M.is_broadcast_mac(dst_mac) or M.is_multicast_mac(dst_mac) ) then
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