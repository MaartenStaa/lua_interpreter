use std::fmt::{Debug, Display};

use miette::{
    Diagnostic, EyreContext, GraphicalReportHandler, LabeledSpan, MietteError, NamedSource,
    SourceCode, SpanContents,
};
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
    type Error = LuaError;

    fn try_from(value: u8) -> std::result::Result<Self, Self::Error> {
        match value {
            0 => Ok(Self::ForLoopLimitIsZero),
            _ => Err(lua_error!("invalid RuntimeError")),
        }
    }
}

/// Generic error (use instead of `miette!`)
#[derive(Error)]
pub struct LuaError {
    pub message: String,
    pub code: Option<String>,
    pub source_code: Option<Box<LuaSourceCode>>,
    pub labels: Option<Vec<LabeledSpan>>,
    pub source: Option<Box<LuaError>>,
    pub chunk: Option<usize>,
}

impl Debug for LuaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let handler = GraphicalReportHandler::new();
        handler.debug(self, f)
    }
}

impl From<RuntimeError> for LuaError {
    fn from(d: RuntimeError) -> Self {
        LuaError {
            message: d.to_string(),
            code: d.code().map(|c| c.to_string()),
            source_code: None,
            source: None,
            labels: None,
            chunk: None,
        }
    }
}

#[derive(Debug, Clone)]
pub enum LuaSourceCode {
    Named(NamedSource<Vec<u8>>),
    Unnamed(Vec<u8>),
}

impl From<NamedSource<Vec<u8>>> for LuaSourceCode {
    fn from(ns: NamedSource<Vec<u8>>) -> Self {
        LuaSourceCode::Named(ns)
    }
}

impl From<Vec<u8>> for LuaSourceCode {
    fn from(v: Vec<u8>) -> Self {
        LuaSourceCode::Unnamed(v)
    }
}

impl SourceCode for LuaSourceCode {
    fn read_span<'a>(
        &'a self,
        span: &miette::SourceSpan,
        context_lines_before: usize,
        context_lines_after: usize,
    ) -> std::result::Result<Box<dyn SpanContents<'a> + 'a>, MietteError> {
        match self {
            LuaSourceCode::Named(ns) => {
                ns.read_span(span, context_lines_before, context_lines_after)
            }
            LuaSourceCode::Unnamed(s) => {
                s.read_span(span, context_lines_before, context_lines_after)
            }
        }
    }
}

pub type Result<T = ()> = std::result::Result<T, LuaError>;

impl Display for LuaError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut target = self;
        while let Some(source) = target.source.as_deref() {
            target = source;
        }
        write!(f, "{}", target.message)
    }
}

impl Diagnostic for LuaError {
    fn diagnostic_source(&self) -> Option<&dyn Diagnostic> {
        self.source.as_deref().map(|s| s as &dyn Diagnostic)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = LabeledSpan> + '_>> {
        self.labels.as_ref().map(|labels| {
            Box::new(labels.clone().into_iter()) as Box<dyn Iterator<Item = LabeledSpan>>
        })
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        self.source_code.as_deref().map(|s| s as &dyn SourceCode)
    }
}

impl LuaError {
    pub(crate) fn new(message: String) -> Self {
        Self {
            message,
            code: None,
            source_code: None,
            labels: None,
            source: None,
            chunk: None,
        }
    }

    pub(crate) fn wrap_err(self, message: String) -> Self {
        Self {
            message,
            code: None,
            source_code: None,
            labels: None,
            source: Some(Box::new(self)),
            chunk: None,
        }
    }

    pub fn with_source(&mut self, source: Self) -> &mut Self {
        self.source = Some(Box::new(source));
        self.source.as_deref_mut().unwrap()
    }

    pub fn with_source_code(mut self, src: impl Into<LuaSourceCode>) -> Self {
        self.source_code = Some(Box::new(src.into()));
        self
    }

    pub(crate) fn with_labels(mut self, labels: impl Into<Vec<LabeledSpan>>) -> Self {
        self.labels = Some(labels.into());
        self
    }
}

pub trait Context {
    fn wrap_err(self, message: impl Into<String>) -> Self
    where
        Self: Sized;

    fn wrap_err_with<M, F>(self, function: F) -> Self
    where
        M: Into<String>,
        F: FnOnce() -> M,
        Self: Sized;
}

impl<T> Context for Result<T> {
    fn wrap_err(self, message: impl Into<String>) -> Result<T> {
        self.map_err(|e| e.wrap_err(message.into()))
    }

    fn wrap_err_with<M, F>(self, function: F) -> Self
    where
        M: Into<String>,
        F: FnOnce() -> M,
        Self: Sized,
    {
        self.map_err(|e| e.wrap_err(function().into()))
    }
}

pub trait IntoLuaError<T> {
    fn into_lua_error(self) -> Result<T>;
}

impl<T, E: std::error::Error + Send + Sync + 'static> IntoLuaError<T>
    for std::result::Result<T, E>
{
    fn into_lua_error(self) -> Result<T> {
        self.map_err(|e| LuaError::new(format!("{e}")))
    }
}

macro_rules! lua_error {
    ($($key:ident = $value:expr,)* $fmt:literal $($arg:tt)*) => {{
        #[allow(unused_mut)]
        let mut error = $crate::error::LuaError::new(format!($fmt $($arg)*));
        $(
        error.$key = $value.into();
        )*
        error
    }}
}

pub(crate) use lua_error;
