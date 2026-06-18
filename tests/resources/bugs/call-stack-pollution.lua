-- "foo" should not stick around on the stack
do
	local x, y = rawlen("foo")
	assert(x == 3)
	assert(y == nil)
end

-- same but with closures
do
	local function foo()
		return 1
	end
	local x, y = foo("test")
	assert(x == 1)
	assert(y == nil)
end
