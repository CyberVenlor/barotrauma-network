local M = {}

function M.Speak()
    local mod1 = require("mod1")
    local m = require("mod1")
    local mod2 = require("mod2")
    
    mod2.Speak()
    mod1.Speak()
    print("mod3")
end

M.Speak()

return M