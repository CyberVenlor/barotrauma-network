-- utility.lua
local utility = (function ()

    local M = {}

    function M.bytes_to_string(bs)
        if type(bs) ~= "table" then error("bytes_to_string: need table") end
        local t, n = {}, #bs
        for i = 1, n do
            local v = bs[i]
            if type(v) ~= "number" then error("bytes_to_string: non-number at "..i) end
            if v ~= math.floor(v) then error("bytes_to_string: non-integer at "..i) end
            if v < 0 or v > 255 then error("bytes_to_string: out of range at "..i) end
            t[i] = string.char(v)
        end
        return table.concat(t)
    end
  
      -- string -> table(byte[])
    function M.string_to_bytes(s)
        if type(s) ~= "string" then error("string_to_bytes: need string") end
        local n = #s
        local o = {}
        for i = 1, n do
            o[i] = string.byte(s, i)
        end
        return o
    end
  
      -- 工具：拼接
    function M.concat_bytes(a,b)
        local o = {}
        for i=1,#a do o[#o+1]=a[i] end
        for i=1,#b do o[#o+1]=b[i] end
        return o
    end
  
      -- 工具：u16
    function M.u16_to_bytes(n)
        return { math.floor(n/256)%256 , n%256 }
    end
    function M.bytes_to_u16(b1,b2)
        return b1*256 + b2
    end

    local hex_map = {
        ["0"]=0, ["1"]=1, ["2"]=2, ["3"]=3, ["4"]=4, ["5"]=5,
        ["6"]=6, ["7"]=7, ["8"]=8, ["9"]=9,
        ["A"]=10, ["B"]=11, ["C"]=12, ["D"]=13, ["E"]=14, ["F"]=15,
        ["a"]=10, ["b"]=11, ["c"]=12, ["d"]=13, ["e"]=14, ["f"]=15,
    }

    function M.hex_byte(a, b)
        local hi, lo = hex_map[a], hex_map[b]
        if not hi or not lo then
            error("bad hex at position")
        end
        return hi * 16 + lo
    end

    return M
end)()
-- mac.lua
local mac = (function ()
    local M = {}

      -- 广播 MAC
    M.BROADCAST = {0xFF,0xFF,0xFF,0xFF,0xFF,0xFF}

    function M.gen_mac()
        local mac = {}
        -- 确保第一个字节的最低位为0（即单播地址）
        mac[1] = math.random(0, 127)  -- 第一个字节范围是 0 到 127，保证最低位为0
        for i = 2, 6 do
             mac[i] = math.random(0, 255)
        end
        return mac
    end

    M.MAC = M.gen_mac()
      -- 转换字节数组为字符串
    function M.mac_to_string(mac)
        return string.format("%02X:%02X:%02X:%02X:%02X:%02X",
            mac[1], mac[2], mac[3], mac[4], mac[5], mac[6])
    end
  
    function M.string_to_mac(s)
        local mac = {}
            -- 用正则抓取两个十六进制
        for hi, lo in s:gmatch("(%x)(%x)") do
            table.insert(mac, utility.hex_byte(hi, lo))
        end
        if #mac ~= 6 then
            error("invalid MAC format")
        end
        return mac
    end

    function M.mac_eq(a,b)
        for i=1,6 do if a[i] ~= b[i] then return false end end
        return true
      end
  
    function M.is_broadcast_mac(m) return M.mac_eq(m, M.BROADCAST) end
    function M.is_multicast_mac(m) return (m[1] % 2) == 1 end -- 低位bit=1

    function M.abort_receive(dst_mac)
        -- 目的过滤：只收自己/广播/组播
        if not (M.mac_eq(dst_mac, M.MAC) or M.is_broadcast_mac(dst_mac) or M.is_multicast_mac(dst_mac) ) then
            return
        end
    
    end

    return M
end)()
-- data_link.lua
local data_link = (function ()
    local M = {}
    local util = utility

    M.ETHERTYPE = {
        IPv4 = 0x0800,
        ARP  = 0x0806,
        LL   = 0x88B5,
    }
  
    function M.transmit_data(src_mac, dst_mac, payload, etype)
        if (type(src_mac) ~= "table" or type(dst_mac) ~= "table" or type(payload) ~= "table") then 
            error("funtion transmit_data receive error type value")
        end
        etype = etype or M.ETHERTYPE.LL
        -- 头部
        local frame = {}
        for i=1,6 do frame[#frame+1] = dst_mac[i] end
        for i=1,6 do frame[#frame+1] = src_mac[i] end
        local etb = util.u16_to_bytes(etype)
        frame[#frame+1] = etb[1]; frame[#frame+1] = etb[2]

        -- 载荷
        frame = util.concat_bytes(frame, payload)
    
        local wire = util.bytes_to_string(frame)
        -- 发到导线
        return wire
    end
  
      -- 接收
  
    function M.receive_data(data) -- data: string (bytes)
  
        local bytes = util.string_to_bytes(data)
        if #bytes < 14 then return end
  
        local dst = {bytes[1],bytes[2],bytes[3],bytes[4],bytes[5],bytes[6]}
        local src = {bytes[7],bytes[8],bytes[9],bytes[10],bytes[11],bytes[12]}
        local et  = util.bytes_to_u16(bytes[13],bytes[14])
        local payload = {}
        for i=15,#bytes do payload[#payload+1] = bytes[i] end
  
        return {
            dst = dst,
            src = src,
            ethertype = et,
            payload = payload,
        }
    end

    return M
end)()
-- serde.lua
local serde = (function ()
    local M = {}

    function M.serialize(t)
        local items={}
        for k,v in pairs(t) do
            local key = (type(k)=="string" and k:match("^%a[%w_]*$")) and (k.."=")
                or ("["..M.serialize(k).."]=")
            local tv=type(v)
            if tv=="string" then
                items[#items+1]=key..('"'..v:gsub('"','\\"')..'"')
            elseif tv=="number" or tv=="boolean" or tv=="nil" then
                items[#items+1]=key..tostring(v)
            elseif tv=="table" then
                error("no nested tables in this tiny version")
            else
                error("unsupported "..tv)
            end
        end
        return "{"..table.concat(items,",").."}"
    end
  
    function M.deserialize(s)
        local t={}
        local body = s:match("^%s*{(.*)}%s*$"); if not body then error("bad literal") end
        local arr_i = 1
        for part in body:gmatch("[^,]+") do
            local k,v = part:match("^%s*([_%a][_%w]*)%s*=%s*(.+)%s*$")
            if k then
            -- 有 key=value
            if v:sub(1,1)=='"' and v:sub(-1)=='"' then
                v = v:sub(2,-2):gsub('\\"','"')
            elseif v=="true" then v=true
            elseif v=="false" then v=false
            elseif v=="nil" then v=nil
            else v = tonumber(v) or v end
            t[k]=v
            else
            -- 没 key，按数组元素
            v = part:match("^%s*(.+)%s*$")
            if v:sub(1,1)=='"' and v:sub(-1)=='"' then
                v = v:sub(2,-2):gsub('\\"','"')
            elseif v=="true" then v=true
            elseif v=="false" then v=false
            elseif v=="nil" then v=nil
            else v = tonumber(v) or v end
            t[arr_i]=v
            arr_i=arr_i+1
            end
        end
        return t
    end

    return M
end)()
-- component.lua
local component = (function ()
    local M = {}

    function M.tx(pin, data)
    ---@diagnostic disable-next-line: undefined-global
        out[pin] = data
    end

    function M.time()
    ---@diagnostic disable-next-line: undefined-global
        return time()
    end

    return M
end)()

-- terminal.lua
local util = utility
local mac = mac
local data_link = data_link
local serde = serde
local component = component

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