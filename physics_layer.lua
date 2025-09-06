local M = {}

local util = require("utility")

function M.tx(bytes)
    local s = util.bytes_to_string(bytes)
    require("core").tx(10, s)
end

function M.rx(string)
    local b = util.string_to_bytes(string)
    return b
end

return M