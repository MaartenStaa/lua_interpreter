use crate::{
    compiler::variable::{Local, Upvalue},
    resolved_ast::BindingId,
};
use std::collections::HashMap;

#[derive(Debug, Default)]
pub(crate) struct Frame {
    pub(crate) locals: Vec<Local>,
    pub(crate) upvalues: HashMap<BindingId, Upvalue>,
}

#[derive(Debug)]
pub(crate) enum BindingLocation<'a> {
    Local { register: u8, local: &'a Local },
    Upvalue { index: u8, attributes: u8 },
}

impl BindingLocation<'_> {
    pub(crate) fn attributes(&self) -> u8 {
        match self {
            BindingLocation::Local { local, .. } => local.attributes,
            BindingLocation::Upvalue { attributes, .. } => *attributes,
        }
    }

    pub(crate) fn is_constant(&self) -> bool {
        self.attributes() & crate::value::LuaVariableAttribute::Constant as u8 != 0
    }
}

#[derive(Debug, Clone, Copy)]
pub(crate) enum UpvalueLocation {
    Local { register: u8 },
    Upvalue { index: u8 },
}

impl Frame {
    pub(crate) fn new() -> Self {
        Self::default()
    }

    pub(crate) fn binding_location(&self, binding: BindingId) -> BindingLocation<'_> {
        if let Some(local) = self
            .locals
            .iter()
            .rev()
            .find(|l| l.binding == Some(binding))
        {
            return BindingLocation::Local {
                register: local.register,
                local,
            };
        }
        let upvalue = self
            .upvalues
            .get(&binding)
            .expect("resolved binding has a register or upvalue");
        BindingLocation::Upvalue {
            index: upvalue.index,
            attributes: upvalue.attributes,
        }
    }
}
