# Lua Rust

This is an implementation of a Rust virtual machine for the Lua language. It is
a complete rewrite, following the [Lua 5.4 language specification][spec].

## Usage

```sh
cargo run --release -- myfile.lua
```

Additional available flags are:

- `--debug-lexer`: Dump each token read by the lexer, and exit. Do not execute the file.
- `--debug-parser`: Same, but for the parser. Dumps out the parsed AST.
- `--disable-optimizer`: Disables the optimization passes on the AST before code
  generation. The optimizer is enabled by default, and performs optimizations
  such as pre-computing constant expressions (e.g. `3 + 4` will be optimized to
  `7`).
- `--print-bytecode`: Print the compiled bytecode before executing it.

## Standard library coverage

The project aims to implement the full Lua 5.4 standard library.

Legend:

- ✅ Fully implemented according to the spec.
- ⚠️ Partially implemented (some functions missing, incorrect, or incomplete).
- 🚫 Functionality not yet implemented.

1. Basic functions \[20/25]
    1. ✅ assert
    2. ✅ collectgarbage
       Note that the Rust Lua VM does not use garbage collection, so this
       function is essentially a stub, while mimicking the behavior of the Lua
       standard library.
    3. 🚫 dofile
    4. ⚠️ error
       The `level` argument is not yet implemented.
    5. ✅ `_G`
    6. ✅ getmetatable
    7. ✅ ipairs
    8. ⚠️ load
       Loading binary chunks is not yet supported.
    9. 🚫 loadfile
    10. ✅ next
    11. ✅ pairs
    12. ✅ pcall
    13. ✅ print
    14. ✅ rawequal
    15. ✅ rawget
    16. ✅ rawlen
    17. ✅ rawset
    18. ✅ select
    19. ✅ setmetatable
    20. ✅ tonumber
    21. ✅ tostring
    22. ✅ type
    23. ✅ `_VERSION`
    24. ✅ warn
    25. ✅ xpcall
2. Coroutine manipulation \[0/8]
    1. 🚫 coroutine.close
    2. 🚫 coroutine.create
    3. 🚫 coroutine.isyieldable
    4. 🚫 coroutine.resume
    5. 🚫 coroutine.running
    6. 🚫 coroutine.status
    7. 🚫 coroutine.wrap
    8. 🚫 coroutine.yield
3. Modules \[1/9]
    1. ⚠️ require
       Loading files is supported, but `package.searchers` is not used
       correctly. Instead, part of the searchers behavior is hardcoded.
    2. ⚠️ package.config
       The fourth and fifth lines are not yet supported.
    3. 🚫 package.cpath
    4. ✅ package.loaded
    5. 🚫 package.loadlib
    6. ⚠️ package.path
       A static value is used, and it does not read from environment variables
       on startup. The value cannot be changed at runtime.
    7. 🚫 package.preload
    8. 🚫 package.searchers
    9. 🚫 package.searchpath
4. String manipulation \[12/17]
    1. ✅ string.byte
    2. ✅ string.char
    3. 🚫 string.dump
    4. ✅ string.find
    5. ⚠️ string.format
       Only the `s` and `d` format specifiers are supported.
    6. ✅ string.gmatch
    7. 🚫 string.gsub
    8. ✅ string.len
    9. ⚠️ string.lower
       Only handles UTF-8 input, and does not use current locale.
    10. ✅ string.match
    11. ✅ string.pack
    12. ✅ string.packsize
    13. ✅ string.rep
    14. ✅ string.reverse
    15. ✅ string.sub
    16. ✅ string.unpack
    17. ⚠️ string.upper
        Only handles UTF-8 input, and does not use current locale.
5. UTF-8 support \[1/6]
    1. 🚫 utf8.char
    2. ✅ utf8.charpattern
    3. 🚫 utf8.codes
    4. 🚫 utf8.codepoint
    5. 🚫 utf8.len
    6. 🚫 utf8.offset
6. Table manipulation \[3/7]
    1. ✅ table.concat
    2. ✅ table.insert
    3. 🚫 table.move
    4. ✅ table.pack
    5. 🚫 table.remove
    6. 🚫 table.sort
    7. ✅ table.unpack
7. Mathematical functions \[24/27]
    1. ✅ math.abs
    2. ✅ math.acos
    3. ✅ math.asin
    4. 🚫 math.atan
    5. ✅ math.ceil
    6. ✅ math.cos
    7. ✅ math.deg
    8. ✅ math.exp
    9. ✅ math.floor
    10. 🚫 math.fmod
    11. ✅ math.huge
    12. ✅ math.log
    13. ✅ math.max
    14. ✅ math.maxinteger
    15. ✅ math.min
    16. ✅ math.mininteger
    17. ✅ math.modf
    18. ✅ math.pi
    19. ✅ math.rad
    20. ✅ math.random
    21. ✅ math.randomseed
    22. ✅ math.sin
    23. ✅ math.sqrt
    24. ✅ math.tan
    25. ✅ math.tointeger
    26. ✅ math.type
    27. ⚠️ math.ult
8. Input and output \[7/21]
    1. ✅ io.close
    2. 🚫 io.flush
    3. 🚫 io.input
    4. 🚫 io.lines
    5. 🚫 io.open
    6. ✅ io.output
    7. 🚫 io.popen
    8. 🚫 io.read
    9. ✅ io.stdin
    10. ✅ io.stderr
    11. ✅ io.stdout
    12. 🚫 io.tmpfile
    13. 🚫 io.type
    14. ✅ io.write
    15. ✅ file:close
    16. 🚫 file:flush
    17. 🚫 file:lines
    18. 🚫 file:read
    19. 🚫 file:seek
    20. 🚫 file:setvbuf
    21. ✅ file:write
9. Operating system facilities \[3/11]
    1. ✅ os.clock
    2. 🚫 os.date
    3. 🚫 os.difftime
    4. 🚫 os.execute
    5. 🚫 os.exit
    6. ✅ os.getenv
    7. 🚫 os.remove
    8. 🚫 os.rename
    9. 🚫 os.setlocale
    10. ✅ os.time
    11. ⚠️ os.tmpname
        Unlike what the manual specifies, this implementation does not create
        the temporary file on Unix systems. Instead, it only returns a unique
        filename.
10. Debug library \[2/15]
    1. 🚫 debug.debug
    2. 🚫 debug.gethook
    3. 🚫 debug.getinfo
    4. 🚫 debug.getlocal
    5. 🚫 debug.getmetatable
    6. 🚫 debug.getregistry
    7. ✅ debug.getupvalue
    8. 🚫 debug.sethook
    9. 🚫 debug.setlocal
    10. 🚫 debug.setmetatable
    11. 🚫 debug.setupvalue
    12. 🚫 debug.setuservalue
    13. 🚫 debug.traceback
    14. ✅ debug.upvalueid
    15. 🚫 debug.upvaluejoin

## C API

This library does not yet implement the Lua C API, but this is planned for the
future.

[spec]: https://www.lua.org/manual/5.4/manual.html
