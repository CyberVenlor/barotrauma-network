local M = {}

local util = require("utility")
local mac = require("mac")
local data_link = require("data_link_layer")
local serde = require("serde")
local physics = require("physics_layer")
local core = require("core")

local has_print_mac = false
local MAC = mac.gen_mac()

function inp(pin, val)
    if pin == 10 then
        local bytes = util.string_to_bytes(val)
        print("bytes: " .. serde.serialize(bytes))
        local frame = data_link.bytes_to_frame(bytes)
        print(frame)
        if frame == nil or mac.abort_receive(MAC, frame.dst_mac) then
            return
        end
        print(serde.serialize(frame))
        if util.bytes_to_string(frame.payload) == "conf" then
            return
        end
        core.tx(11, util.bytes_to_string(frame.payload))
        local send_package = {
            dst_mac = frame.src_mac,
            src_mac = MAC,
            ethertype = data_link.ETHERTYPE.IPv4,
            payload = util.string_to_bytes("conf")
        }
        local bytes = data_link.frame_to_bytes(send_package)
        physics.tx(bytes)
    elseif pin == 11 then
        local input = serde.deserialize(val)
        print("input: " .. val)
        if input == nil then return end
        local send_frame = {
            dst_mac = mac.string_to_mac(input.mac),
            src_mac = MAC,
            ethertype = data_link.ETHERTYPE.IPv4,
            payload = util.string_to_bytes(input.payload)
        }
        local bytes = data_link.frame_to_bytes(send_frame)
        physics.tx(bytes)
    end
end
 
function upd()
    if has_print_mac == false then
        core.tx(11,  mac.mac_to_string(MAC))
        has_print_mac = true
    end
end

function M.input(pin, val)
    inp(pin, val)
end

--{mac="AC:F9:3A:8E:BE:10",payload="fuck"}

return M