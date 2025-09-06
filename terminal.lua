local M = {}

local util = require("utility")
local serde = require("serde")
local mac = require("mac")
local ip = require("ip")
local core = require("core")
local physics = require("physics_layer")
local data_link = require("data_link_layer")
local network_layer = require("network_layer")


local stored_config = nil

function inp(pin, val)
    if pin == 10 then
        local payload = network_layer.rx(data_link.rx(physics.rx(val)))
        if payload == nil then return end
        --print(serde.serialize(frame))
        core.tx(11, util.bytes_to_string(payload))
    elseif pin == 11 then
        local input = serde.deserialize(val)
        if input == nil then return end
        print(network_layer.tx(ip.string_to_ipv4(input.ip), util.string_to_bytes(input.payload)))
    elseif pin == 12 then -- config
        local config = serde.deserialize(val)
        if config == nil then return end
        if stored_config ~= val then
            stored_config = val
            core.tx(11, val)
        end
        if config.mac ~= nil then
            data_link.MAC = mac.string_to_mac(config.mac)
        end
        if config.ip ~= nil then
            network_layer.IP = ip.string_to_ipv4(config.ip)
        end
    end
end
 
function upd()
end

function M.input(pin, val)
    inp(pin, val)
end

-- {mac="90:C5:60:58:B7:BD",payload="fuck"}
-- {ip="192.168.1.4",payload="fuck"}

return M