-- Uses before a later capture must already be compiled as potentially captured.
local x = 10
local before = x
local function change() x = x + 1 end
change()
assert(before == 10 and x == 11)

-- Each shadow has a separate identity, even after an earlier closure captured x.
local function shadows()
    local x = 1
    local function first() return x end
    local x = 2
    local function second() x = x + 1; return x end
    assert(first() == 1 and second() == 3 and x == 3)
end
shadows()

-- Intermediate functions forward captures used only by their descendants.
local function outer()
    local value = 4
    return function() return function() value = value + 1; return value end end
end
local inner = outer()()
assert(inner() == 5 and inner() == 6)

-- Ordinary initializers see the old binding; local functions see themselves.
local original = 7
local original = function() return original end
assert(original() == 7)
local function factorial(n)
    if n == 0 then return 1 end
    return n * factorial(n - 1)
end
assert(factorial(5) == 120)

-- All numeric-for header expressions see the outer i.
local i = 3
local count = 0
for i = 1, i, i - 2 do count = count + i end
assert(count == 6 and i == 3)

-- Repeat conditions share the body's scope.
local count = 0
repeat
    local done = count == 2
    count = count + 1
until done
assert(count == 3)

-- Global uses capture the lexical _ENV, including through intermediate functions.
local check = assert
local function environment()
    local _ENV = { answer = 42 }
    return function() return function() return answer end end
end
check(environment()()() == 42)

-- Reusing a register after a captured binding goes out of scope is safe.
local saved
 do
    local captured = 12
    saved = function() return captured end
end
local uncaptured = 99
local copy = uncaptured
assert(copy == 99 and saved() == 12)

local invalid, message = load("local =")
assert(invalid == nil and type(message) == "string")
