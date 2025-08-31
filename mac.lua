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
        table.insert(mac, require("utility").hex_byte(hi, lo))
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
        return true
    end
    
    return false
end

return M
