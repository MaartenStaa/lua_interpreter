use super::LuaNumber;

#[derive(Debug, Clone, PartialEq)]
pub struct LuaFunctionDefinition {
    pub name: Option<String>,
    pub chunk: usize,
    pub ip: u16,
    pub upvalues: usize,
}

#[derive(Debug, Clone, PartialEq)]
pub enum LuaConst {
    Marker,
    Nil,
    Boolean(bool),
    Number(LuaNumber),
    String(Vec<u8>),
    Function(LuaFunctionDefinition),
}
