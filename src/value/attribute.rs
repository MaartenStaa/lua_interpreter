use miette::miette;

#[derive(Debug, Clone, Copy)]
#[repr(u8)]
pub enum LuaVariableAttribute {
    Constant = 1,
    ToBeClosed = 2,
}

impl TryFrom<u8> for LuaVariableAttribute {
    type Error = miette::Report;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            1 => Ok(Self::Constant),
            2 => Ok(Self::ToBeClosed),
            _ => Err(miette!("invalid LuaVariableAttribute")),
        }
    }
}
