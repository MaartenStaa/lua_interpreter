use crate::{
    compiler::frame::UpvalueLocation,
    token::Span,
    value::{LuaString, LuaVariableAttribute},
};

#[derive(Debug, Clone)]
pub(crate) struct Local {
    pub(crate) name: LuaString,
    pub(crate) depth: u8,
    pub(crate) span: Option<Span>,
    pub(crate) attributes: u8,
}

impl Local {
    pub(crate) fn is_constant(&self) -> bool {
        self.attributes & (LuaVariableAttribute::Constant as u8) != 0
    }
}

#[derive(Debug, Clone)]
pub(crate) struct Upvalue {
    pub(crate) index: u8,
    pub(crate) source: UpvalueLocation,
    pub(crate) attributes: u8,
}
