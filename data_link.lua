local M = {}
local util = require("utility")

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