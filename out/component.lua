-- component.lua
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