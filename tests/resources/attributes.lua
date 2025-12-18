-- cannot assign to const
local r, m = load([[
  local a <const> = 5
  a = 10
]])

assert(type(r) == "nil", "r should be nil, found " .. type(r))
assert(string.find(m, "attempt to assign to const"))

-- same via upvalue
local r, m = load([[
  local a <const> = 5
  local function f()
    a = 10
  end
]])

assert(type(r) == "nil", "r should be nil, found " .. type(r))
assert(string.find(m, "attempt to assign to const"))
