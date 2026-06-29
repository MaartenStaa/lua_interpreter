use crate::LuaState;

/// A native Lua function is a Rust function that can be called from Lua code. It takes a mutable
/// reference to the `LuaState` and returns a `Result` containing the number of return values
/// pushed onto the stack.
pub type NativeLuaFunction = fn(&mut LuaState) -> crate::Result<u16>;
