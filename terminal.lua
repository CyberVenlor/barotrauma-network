local util = require("utility")
local mac = require("mac")
local data_link = require("data_link")
local serde = require("serde")
local component = require("component")

local has_print_mac = false

function inp(pin, val)
    if pin == 10 then
        local received = data_link.receive_data(val)
        if received == nil or mac.abort_receive(received.dst) then
            return
        end
        if util.bytes_to_string(received.payload) == "conf" then
            return
        end
        component.tx(11, util.bytes_to_string(received.payload))
        data_link.transmit_data(mac.MAC, received.src, util.string_to_bytes("conf"), data_link.ETHERTYPE.IPv4)
    elseif pin == 11 then
        local input = serde.deserialize(val)
        local tx = data_link.transmit_data(mac.MAC, mac.string_to_mac(input.mac), util.string_to_bytes(input.payload), data_link.ETHERTYPE.IPv4)
        print(tx)
        component.tx(10, tx)
    end
end
 
function upd()
    if has_print_mac == false then
        component.tx(11, "mac: " .. mac.mac_to_string(mac.MAC))
        has_print_mac = true
    end
end

--{mac="AC:F9:3A:8E:BE:10",payload="fuck"}