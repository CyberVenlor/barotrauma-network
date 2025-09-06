local M = {}

local util = require("utility")
local mac = require("mac")
local data_link = require("data_link_layer")
local serde = require("serde")
local physics = require("physics_layer")
local core = require("core")
local network_layer = require("network_layer")
local ip = require("ip")

local stored_config = nil

function inp(pin, val)
    if pin == 10 then
        print(val)
        local frame = data_link.rx(physics.rx(val))
        if frame == nil then return end
        print(serde.serialize(frame))
        if util.bytes_to_string(frame.payload) == "conf" then return end
        --debug
        core.tx(11, util.bytes_to_string(frame.payload))
        data_link.tx(frame.src_mac, util.string_to_bytes("conf"))
    elseif pin == 11 then
        local input = serde.deserialize(val)
        if input == nil then return end
        data_link.tx(mac.string_to_mac(input.mac), util.string_to_bytes(input.payload))
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

--{mac="AC:F9:3A:8E:BE:10",payload="fuck"}

return M