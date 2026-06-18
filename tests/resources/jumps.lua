-- simple forward goto
do
  local foo = 1
  goto bar
  foo = 2
  ::bar::
  assert(foo == 1)
end

-- backward goto
do
  local foo = 1
  ::baz::
  if foo < 3 then
    foo = foo + 1
    goto baz
  end
  assert(foo == 3)
end

-- goto inside blocks
do
  local foo = 1
  do
    goto inside
    foo = 2
    ::inside::
    foo = foo + 1
  end
  assert(foo == 2)
end

-- goto higher scopes
do
  local foo = 1
  do
    do
      goto higher
      foo = foo + 1
    end
  end
  ::higher::
  assert(foo == 1)
end

-- cannot goto narrower scope
do
  local r, m = load([[
    local x = 1
    goto inside
    do
      ::inside::
      x = x + 1
    end
  ]])
  assert(type(r) == "nil", "r should be nil, found " .. type(r))
  assert(string.find(m, "no visible label 'inside'"), m)
end

-- cannot goto non-existent label
do
  local r, m = load([[
    goto nowhere
  ]])
  assert(type(r) == "nil", "r should be nil, found " .. type(r))
  assert(string.find(m, "no visible label 'nowhere'"), m)
end

-- cannot goto across function boundaries
do
  local r, m = load([[
    goto outside
    local function foo()
      ::outside::
    end
  ]])
  assert(type(r) == "nil", "r should be nil, found " .. type(r))
  assert(string.find(m, "no visible label 'outside'"), m)
end

-- cannot jump into scope of local
do
  local r, m = load([[
    do
      goto inside
      local x = 10
      ::inside::
      print(x)
    end
  ]])
  assert(type(r) == "nil", "r should be nil, found " .. type(r))
  assert(string.find(m, "jumps into the scope of local 'x'"), m)
end

-- but it's okay if the local isn't used
do
  local r, m = load([[
    do
      goto inside
      local x = 10
      ::inside::
    end
  ]])
  assert(type(r) == "function", "r should be a function, found " .. type(r) .. ", because: " .. tostring(m))
  assert(type(m) == "nil", "m should be nil, found " .. tostring(m))
end
