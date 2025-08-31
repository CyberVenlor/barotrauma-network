local mac = require("mac")
local data_link = require("data_link")
local component = require("component")

local mac_table = {}

local function broadcast(data, exception)
    for i = 1, 30 do
        if i ~= exception then
            component.tx(i, data)
        end
    end
end

function inp(pin, val)
    print(val)
    local received = data_link.receive_data(val)
    if received == nil then
        return
    end
    local src = mac.mac_to_string(received.src)
    -- learn new mac
    mac_table[src] = {pin = pin, time = component.time()}
    print("src_mac: " .. src .. ", dst_mac: ".. mac.mac_to_string(received.dst) .. ", pin: " .. pin .. ", time: ", component.time())
    -- broadcast
    if mac.is_broadcast_mac(received.dst) or mac.is_multicast_mac(received.dst) then
        print("broadcast")
        broadcast(val, pin)
        return
    end
    local dst = mac.mac_to_string(received.dst)
    -- 泛洪
    local pair = mac_table[dst]
    if pair == nil then
        print("泛洪")
        broadcast(val, pin)
        return
    end
    -- 正常播送
    component.tx(pair.pin, val)
end

function upd()
    -- 老化
    for key, value in pairs(mac_table) do
        if component.time() - value.time > 60 then
            mac_table[key] = nil
        end
    end
end