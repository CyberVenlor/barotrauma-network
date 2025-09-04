local M = {}

function M.tx(bytes)
    local s = require("utility").bytes_to_string(bytes)
    require("core").tx(10, s)
end

return M