# Instruction set

This document describes the instruction set of the virtual machine. The virtual
machine is a stack based machine, which means that almost all instructions
operate on the stack. The stack is a LIFO (Last In First Out) data structure
that stores values (instances of `LuaValue`). Any `LuaValue` is assumed to be
safe to `clone()`. Any value that should be passed by reference is a
`LuaObject`, and is passed around on the stack as a `LuaValue::Object`.

## Special values

### `Marker`

The `Marker` value is a special value that is used to mark the beginning of a
sequence of values. It is used to separate the arguments to a function from the
local variables of the function. It is also used in assignment statements, to
ensure an equal number of values to the number of variables.

## Conventions

### Local variables

Local variables are stored on the stack as values. The index of a local variable
is relative to the current stack frame. The frame pointer is the index of the
first argument in the stack. The local variables are registered at compile time,
and are accessed purely by index, meaning no lookup is required at runtime.

### Global variables

Global variables are stored in a table in the virtual machine's global state.
The index of a global variable corresponds to a key in the constants table,
where the value is the name of the global variable. The global variables are
looked up at runtime by index, but their name may be looked up at runtime if
desired (e.g. for debugging purposes).

### Function calls and variadics

When a function is called, a marker is pushed onto the stack followed by the
arguments to the function. This allows determining the number of arguments at
runtime, when passing a variadic number of arguments to a function.

The marker is discarded (all values above it are shifted down) when the function
is called, and the frame pointer is set to the index of where the arguments
start. This allows the function to access its arguments by indexing the stack
from the frame pointer.

In the case of a variadic function, the variadic arguments are stored in a
sequence table, which is pushed onto the stack as a local variable. The
`AlignVararg` instruction is used to collect the variadic arguments into a
sequence table.

For example, a `function foo(bar, baz, ...)` when called with `foo(1, 2, 3, 4)`,
would have the following local variables:

```
[1, 2, {3, 4}]
```

### Closures and upvalues

Closures are functions that can capture variables from their enclosing scope. At
compilation time, the compiler determines which variables are captured by the
closure, and embeds information about this in the `LoadClosure` instruction. At
runtime, this instruction converts the local value on the stack into an upvalue,
a reference to the captured variable. The closure object iself also keeps a list
of upvalues, which are used to access the captured variables.

To access an upvalue, the closure uses the `GetUpval` and `SetUpval`
instructions, both of which take an index as an argument and operate on the
upvalue list of the closure, which is saved in the call frame.

## Instructions

### Stack operations

#### `LoadConst`

Reads a constant index from the bytes directly following the instruction and
looks up the value of the constant from the virtual machine's constant table.

The `LuaConst` value is then pushed onto the stack.

#### `LoadClosure`

Reads a function index from the bytes directly following the instruction and
looks up the function from the virtual machine's function table. The function
contains information about the number of upvalues it captures.

For each captured upvalue, the instruction reads two bytes. The first indicates
whether this upvalue refers to a `local` in the parent scope (as opposed to an
already captured upvalue), and the second is the index of the local variable or
upvalue.

In the case of a local, the virtual machine will convert the local variable on
the stack into an upvalue, and replace the local variable with the upvalue. In
the case of an upvalue, the virtual machine will simply fetch the upvalue from
the current call frame. In both cases, the upvalue is then stored in the closure
object.

The closure object is pushed onto the stack.

#### `Pop`

Pops the top value from the stack, discarding it.

#### `Discard`

Keeps popping values from the stack until a `Marker` is found, discarding all of
them.

#### `Swap`

Reads an offset from the byte directly following the instruction and swaps the
top value with the value at the offset from the top of the stack.

For example, if the stack is `[1, 2, 3, 4]` and the instruction is `Swap 2`,
the stack will become `[1, 3, 2, 4]`.

#### `Align`

Reads a number from the byte directly following the instruction, and manipulates
the stack so that there are exactly that many values from the position of the
latest `LuaValue::Marker` to the top of the stack. The `Align` operation
discards both any values above the marker and the marker itself. If there are
fewer than `n` values above the marker, the operation will pad the stack with
`nil` values.

For example, if the stack is `[1, 2, Marker, 3, 4]` and the instruction is
`Align 3`, the stack will become `[1, 2, 3, 4, nil]`. Conversely, if the stack
is `[1, 2, Marker, 3, 4]` and the instruction is `Align 1`, the stack will
become `[1, 2, 3]`.

#### `AlignVararg`

This operation is similar to `Align`, but any extraneous values beyond `n` are
collected into a sequence table, which is then pushed onto the stack. This is
used to collect variadic arguments into a single table.

For example, if the stack is `[1, 2, Marker, 3, 4, 5]` and the instruction is
`AlignVararg 1`, the stack will become `[1, 2, 3, {4, 5}]`, where in reality
`{4, 5}` is a pointer to a table on the heap. The table containing the variadic
arguments are stored as a "local" (see `GetLocal` and `LoadVararg` below).

#### `DupFromMarker`

Reads an offset from the byte directly following the instruction, and duplicates
the value at the offset from the latest `Marker` on the stack. The duplicated
value is pushed onto the stack.

For example, if the stack is `[1, 2, Marker, 3, 4, 5]` and the instruction is
`DupFromMarker 1`, the stack will become `[1, 2, Marker, 3, 4, 5, 3]`.

### Binary operations

#### Arithmetic

The arithmetic operations are `Add`, `Sub`, `Mul`, `Div`, `Mod`, `Pow`, `IDiv`,
`BAnd`, `BOr`, `BXor`, `Shl`, and `Shr`. All of these operations pop two values
from the stack, perform the operation, and push the result back onto the stack.

#### Comparison

The comparison operations are `Eq`, `Ne`, `Lt`, `Le`, `Gt`, and `Ge`. All of
these operations pop two values from the stack, perform the comparison, and push
the result back onto the stack.

#### Concatenation

The concatenation operation is `Concat`. It pops two values from the stack,
concatenates them, and pushes the result back onto the stack.

### Unary operations

#### `Neg`

Pops a value from the stack, negates it, and pushes the result back onto the
stack. For example, if the stack is `[1]`, the stack will become `[-1]`.

#### `Not`

Pops a value from the stack, negates it, and pushes the result back onto the
stack. For example, if the stack is `[true]`, the stack will become `[false]`.

#### `Len`

Pops a value from the stack, gets its length, and pushes the result back onto
the stack. For example, if the stack is `["abc"]`, the stack will become
`[3]`.

### `BNot`

Pops a value from the stack, bitwise negates it, and pushes the result back onto
the stack. For example, if the stack is `[5]`, the stack will become `[-6]`.

## Variable operations

### `SetGlobal`

Reads an index from the bytes directly following the instruction. It pops the
top value from the stack and sets the global variable at the index to the value.

### `GetGlobal`

Reads an index from the bytes directly following the instruction. It looks up
the global variable at the index and pushes the value onto the stack. If the
variable does not exist, it pushes `nil`.

### `SetLocal`

Reads a stack index from the bytes directly following the instruction. It pops
the top value from the stack and sets the local variable at the index to the
value. The index here is relative to the current stack frame.

If the value on the stack refers to an upvalue, the upvalue is set instead.

### `GetLocal`

Reads a stack index from the bytes directly following the instruction. It looks
up the local variable at the index and pushes the value onto the stack. The
index here is relative to the current stack frame.

If the local variable refers to an upvalue, the inner value of the upvalue is
extracted and pushed onto the stack.

### `SetUpval`

Reads an upvalue index from the bytes directly following the instruction. It
pops the top value from the stack and sets the upvalue at the index in the
current call frame to the popped value.

### `GetUpval`

Reads an upvalue index from the bytes directly following the instruction. It
looks up the upvalue at the index in the current call frame and pushes the value
onto the stack.

### `LoadVararg`

This is similar to `GetLocal`, but it expects the local variable to be a
sequence table containing variadic arguments. It pushes each of the variadic
arguments onto the stack.

## Table operations

### `NewTable`

Creates a new table and pushes it onto the stack.

### `SetTable`

Pops a key and a value from the stack, and sets the value of the key in the
table at the top of the stack to the value. The table is kept on the stack.

For example, if the stack is `[table, key, value]`, the table will become
`[table]` with the key-value pair set.

### `GetTable`

Pops a key and a table from the stack, and looks up the value of the key in the
table. The value is pushed onto the stack. If the key does not exist in the
table, `nil` is pushed.

For example, if the stack is `[table, key]`, the stack will become `[value]`.

## Function operations

### `Call`

Pops a function from the stack, and then determines the number of arguments to
pass to the function by checking up the stack for a `Marker`. The marker is then
discarded.

The operation pushes a new call frame onto the dedicated call frames stack,
storing the name of the function (if any, for the purposes of a stack trace in
the case of an error), the frame pointer (the index of the first argument in the
stack), and the return address (the instruction pointer after the `Call`
instruction).

For example, if the stack is `[1, 2, marker, 3, function]`, and the `Call`
instruction is at instruction pointer 10, the call frame stack will become
`[CallFrame("function", 2, 11)]`, and the stack will become `[1, 2, 3]`. This
way, the function can access its arguments by indexing the stack from the frame
pointer.

### `Return`

Gathers the return values from the top of the stack to the latest `Marker`, and
pops the call frame from the call frame stack. The stack pointer is then reset
to the frame pointer of the popped call frame, before pushing the return values
back onto the stack.

For example, if the stack is `[1, 2, 3, Marker, 4, 5]`, and the call frame stack
is `[CallFrame("function", 2, 11)]`, the stack will become `[1, 2, 4, 5]`.

## Control flow

### `Jmp`

Reads an instruction pointer from the bytes directly following the instruction,
and sets the instruction pointer to that value. This is used for unconditional
jumps.

### `JmpTrue`

Reads an instruction pointer from the bytes directly following the instruction,
and pops a value from the stack. If the value is true, the instruction pointer
is set to the value. Otherwise, the instruction pointer is incremented by the
size of the instruction.

### `JmpFalse`

The same as `JmpTrue`, but the jump is taken if the value is false.
