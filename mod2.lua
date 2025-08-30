local M = {}

function M.Speak()
    local mod1 = require("mod1")
    mod1.Speak()
    print("mod2")
end

return M