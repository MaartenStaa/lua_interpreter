-- tables are compared by reference, not by value
do
	local a = {}
	local b = {}
	assert(a ~= b, "different tables should not be equal")
end

-- ensure self-referential tables don't cause a stack overflow
do
	local a = {}
	a.foo = a
	local b = {}
	b.foo = b
	assert(a ~= b, "different self-referential tables should not be equal")
end
