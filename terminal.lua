local M = {}

local util = require("utility")
local mac = require("mac")
local data_link = require("data_link_layer")
local serde = require("serde")
local physics = require("physics_layer")
local core = require("core")

local has_print_mac = false

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
    end
end
 
function upd()
    if has_print_mac == false then
        core.tx(11,  mac.mac_to_string(data_link.MAC))
        has_print_mac = true
    end
end

function M.input(pin, val)
    inp(pin, val)
end

--{mac="AC:F9:3A:8E:BE:10",payload="fuck"}

return M