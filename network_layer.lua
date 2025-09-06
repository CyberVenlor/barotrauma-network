local M = {}
local serde = require("serde")
local core = require("core")
local ip = require("ip")
local mac_mod = require("mac")
local util = require("utility")
local data_link = require("data_link_layer")
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
        
        return "ARP"
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