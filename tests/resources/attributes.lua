-- cannot assign to const
do
	local r, m = load([[
    local a <const> = 5
    a = 10
  ]])
	assert(type(r) == "nil", "r should be nil, found " .. type(r))
	assert(string.find(m, "attempt to assign to const"))
end

-- same via upvalue
do
	local r, m = load([[
    local a <const> = 5
    local function f()
      a = 10
    end
  ]])
	assert(type(r) == "nil", "r should be nil, found " .. type(r))
	assert(string.find(m, "attempt to assign to const"))
end

-- close metamethod
do
	local closed = false
	do
		local foo <close> = setmetatable({}, {
			__close = function()
				closed = true
			end,
		})
	end
	assert(closed == true, "close metamethod was not called")
end

-- nil and false are ignored for close
do
	local foo <close> = nil
	local bar <close> = false
end

-- other values without a __close metamethod raise an error
do
	local r, m = pcall(function()
		local foo <close> = 10
	end)
	assert(r == false, "should have raised an error")
	assert(string.find(m, "non-closable", nil, true))
end

-- to-be-closed values are closed in reverse order
do
	local log = {}
	local function make_closer(name)
		return setmetatable({}, {
			__close = function()
				table.insert(log, name)
			end,
		})
	end

	do
		local a <close> = make_closer("a")
		local b <close> = make_closer("b")
		local c <close> = make_closer("c")
	end

	assert(#log == 3, "expected 3 close calls, got " .. #log)
	assert(log[1] == "c" and log[2] == "b" and log[3] == "a", "close order incorrect")
end

-- close in loops
do
	local count = 0
	for _ = 1, 5 do
		local foo <close> = setmetatable({}, {
			__close = function()
				count = count + 1
			end,
		})
	end
	assert(count == 5, "expected 5 close calls, got " .. count)

	count = 0
	for k, v in ipairs({ 1, 2, 3, 4, 5 }) do
		local foo <close> = setmetatable({}, {
			__close = function()
				count = count + v
			end,
		})
	end
	assert(count == 15, "expected close calls with sum(1..=5), got " .. count)
end

-- handle jumps out of to-be-closed scopes
do
	local log = {}
	local function make_closer(name)
		return setmetatable({ name = name }, {
			__close = function()
        print("closing", name)
				table.insert(log, name)
			end,
		})
	end

	local function test_break()
		for i = 1, 3 do
			local foo <close> = make_closer("loop" .. i)
			if i == 2 then
				break
			end
		end
	end

	local function test_goto_forward()
		do
			local foo <close> = make_closer("goto")
			goto target
			error("should not reach here")
		end
    local bar <close> = make_closer("should not close (unreachable)")
		-- jump over end of scope
		::target::
	end

  local function test_goto_backward()
    local x = false
    ::target::
    if x then
      return
    end
    x = true
    do
      local foo <close> = make_closer("goto")
      goto target
      error("should not reach here")
    end
  end

  local function test_goto_same_scope()
    local x = false
    local foo <close> = make_closer("goto")
    goto target
    error("should not reach here")
    ::target::
    assert(x == false)
    assert(foo.name == "goto")
  end

	log = {}
	test_break()
	assert(#log == 2 and log[2] == "loop2", "break did not close correctly")

	log = {}
	test_goto_forward()
	assert(#log == 1 and log[1] == "goto", "goto did not close correctly")

	log = {}
	test_goto_backward()
	assert(#log == 1 and log[1] == "goto", "goto did not close correctly")

	log = {}
	test_goto_same_scope()
	assert(#log == 1 and log[1] == "goto", "goto did not close correctly")
end
