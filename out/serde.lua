-- serde.lua
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