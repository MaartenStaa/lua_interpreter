-- relation between _ENV and _G
assert(_ENV == _G, "_ENV and _G should be the same table")
assert(_ENV._G == _G, "_G is accessible through _ENV")
assert(_G._ENV == nil, "_ENV is not accessible through _G")

-- declare a global variable
A = {}
assert(A == _ENV.A, "A should be accessible via _ENV")
assert(A == _G.A, "A should be accessible via _G")

-- but _ENV is a regular variable name, so you can set a custom _ENV
do
	local assert = assert
	local _ENV = {}
	B = {}

	assert(B == _ENV.B, "B should be accessible via the local _ENV")
end
assert(_ENV.B == nil, "B should not be accessible via the outer _ENV")

-- load with custom environment
do
	local env = { C = {} }
	local f = assert(load("return C, print", nil, "t", env))
	local c, p = f()
	assert(c == env.C, "C should be accessible via the custom environment")
	assert(p == nil, "print should not be accessible via the custom environment")
	assert(_G.C == nil, "C should not be accessible via _G")
end

-- load gets the same global environment by default
do
	local f = assert(load("D = 1234"))
	local d = f()
	assert(d == nil, "loaded chunk should not return anything")
	assert(D == 1234, "D should be accessible via _G")
	assert(D == _ENV.D, "D should be accessible via _ENV")
end

-- custom env affects access to _G
do
	local f = assert(load("return _G", nil, "t", {}))
	local g = f()
	assert(g == nil, "_G should not be accessible via the custom environment")
end
