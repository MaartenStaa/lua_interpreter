use crate::LuaState;

/// A Lua function JIT compiled using Cranelift. It takes a mutable pointer to the [`LuaState`] and
/// returns a [`Result`] containing the number of return values pushed onto the stack.
pub type JitLuaFunction = fn(*mut LuaState) -> crate::Result<usize>;

/// A native Lua function is a Rust function that can be called from Lua code. It takes a mutable
/// reference to the [`LuaState`] and returns a [`Result`] containing the number of return values
/// pushed onto the stack.
pub type NativeLuaFunction = fn(&mut LuaState) -> crate::Result<usize>;
