local x = 0
local function foo()
	local function bar()
		x = x + 1
	end
	return bar
end

local function baz()
	x = x + 1
end

local bar = foo()
baz()
bar()

assert(x == 2)
