use crate::{error::lua_error, token::Span, value::LuaString, vm::JumpAddr};

#[derive(Debug)]
pub(crate) struct BlockOptions {
    pub(crate) is_loop: bool,
    pub(crate) loop_scope_depth: u8,
    pub(crate) new_scope: bool,
}

#[derive(Debug, Default)]
pub(crate) struct BreakJump {
    pub(crate) addr: JumpAddr,
}

#[derive(Debug)]
pub(crate) struct NewLocalsAfterGoto {
    pub(crate) goto: (LuaString, Span),
    pub(crate) local: (LuaString, Option<Span>),
}

#[derive(Debug, Default)]
pub(crate) struct BlockResult {
    pub(crate) breaks: Vec<BreakJump>,
    any_new_locals: Option<NewLocalsAfterGoto>,
}

impl BlockResult {
    pub(crate) fn new() -> Self {
        Self {
            breaks: vec![],
            any_new_locals: None,
        }
    }

    pub(crate) fn extend(&mut self, other: Self) {
        self.breaks.extend(other.breaks);
        if self.any_new_locals.is_none() {
            self.any_new_locals = other.any_new_locals;
        }
    }

    pub(crate) fn with_breaks(mut self, breaks: Vec<BreakJump>) -> Self {
        self.breaks = breaks;
        self
    }

    pub(crate) fn with_new_locals(mut self, any_new_locals: Option<NewLocalsAfterGoto>) -> Self {
        self.any_new_locals = any_new_locals;
        self
    }

    pub(crate) fn assert_no_new_locals(&self) -> crate::Result {
        if let Some(new_local) = &self.any_new_locals {
            let mut labels = vec![new_local.goto.1.labeled("this goto statement")];
            if let Some(span) = new_local.local.1 {
                labels.push(span.labeled("this local variable definition"));
            }
            return Err(lua_error!(
                labels = labels,
                "<goto {}> jumps into the scope of local '{}'",
                String::from_utf8_lossy(&new_local.goto.0),
                String::from_utf8_lossy(&new_local.local.0)
            ));
        }
        Ok(())
    }
}
