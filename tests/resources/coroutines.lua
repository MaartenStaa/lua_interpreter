-- can create and run a coroutine
do
	local a = coroutine.create(function()
		return 1, 2, 3
	end)
	assert(type(a) == "thread")
	local b = { coroutine.resume(a) }
	assert(#b == 4)
	assert(b[1] == true)
	assert(b[2] == 1)
	assert(b[3] == 2)
	assert(b[4] == 3)
end

-- coroutines can receive arguments
do
	local co = coroutine.create(function(x, y)
		return x + y
	end)
	local ok, result = coroutine.resume(co, 5, 7)
	assert(ok)
	assert(result == 12)
end

-- coroutines may have upvalues
do
	local x = 10
	local co = coroutine.create(function()
		x = 20
		return x
	end)
	local ok, result = coroutine.resume(co)
	assert(ok)
	assert(result == 20)
	assert(x == 20)
end

-- coroutines may yield and be resumed
do
	local co = coroutine.create(function()
		local a = coroutine.yield(1)
		local b = coroutine.yield(a + 1)
		return b + 1
	end)
	local ok, result = coroutine.resume(co)
	assert(ok)
	assert(result == 1)
	ok, result = coroutine.resume(co, 10)
	assert(ok)
	assert(result == 11)
	ok, result = coroutine.resume(co, 20)
	assert(ok)
	assert(result == 21)
end

print("ok")
