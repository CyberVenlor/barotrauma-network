local mac = require("mac")
local data_link = require("data_link_layer")
local core = require("core")
local util = require("utility")

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
    --print(require("serde").serialize(mac_table))
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