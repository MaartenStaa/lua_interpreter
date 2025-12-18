-- declare a global function
function GlobalFunction()
	return "I am a global"
end

assert(GlobalFunction ~= nil)
assert(type(GlobalFunction) == "function")
assert(_ENV.GlobalFunction ~= nil)
assert(_G.GlobalFunction ~= nil)
assert(GlobalFunction == _ENV.GlobalFunction and _ENV.GlobalFunction == _G.GlobalFunction)
assert(GlobalFunction() == "I am a global")

-- declare a local function
local function local_function()
	return "I am a local"
end

assert(local_function ~= nil)
assert(_ENV.local_function == nil)
assert(_G.local_function == nil)
assert(type(local_function) == "function")
assert(local_function() == "I am a local")

-- declare a closure over the local function
local function closure()
	return local_function()
end

assert(closure ~= nil)
assert(_ENV.closure == nil)
assert(_G.closure == nil)
assert(type(closure) == "function")
assert(closure() == "I am a local")

-- attempt to call a non-existent function
assert(pcall(function()
	return non_existent_function()
end) == false)

-- declare a function that returns multiple values
local function multiple_returns()
	return 1, 2, 3
end

local a, b, c, d = multiple_returns()
assert(a == 1 and b == 2 and c == 3 and d == nil)
local e, f = (multiple_returns())
assert(e == 1 and f == nil)
