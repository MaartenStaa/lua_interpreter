-- ensure we can allocate more registers than known ahead of time (via table.unpack)
local function call(f, args)
	return f(table.unpack(args, 1, args.n))
end

local lim = 20
local i, a = 1, {}
while i <= lim do
	a[i] = i + 0.3
	i = i + 1
end

local function f(a, b, c, d, ...)
	local more = { ... }
	assert(a == 1.3 and more[1] == 5.3 and more[lim - 4] == lim + 0.3 and not more[lim - 3])
end

call(f, a)
