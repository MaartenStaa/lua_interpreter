use miette::{miette, Diagnostic};
use thiserror::Error;

#[derive(Debug, Diagnostic, Error)]
#[repr(u8)]
pub(crate) enum RuntimeError {
    #[error("for loop limit is zero")]
    #[diagnostic(code = "for_loop_limit_is_zero")]
    ForLoopLimitIsZero,
}

impl From<RuntimeError> for u8 {
    fn from(e: RuntimeError) -> u8 {
        e as u8
    }
}

impl TryFrom<u8> for RuntimeError {
    type Error = miette::Report;

    fn try_from(value: u8) -> Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::ForLoopLimitIsZero),
            _ => Err(miette!("invalid RuntimeError")),
        }
    }
}
