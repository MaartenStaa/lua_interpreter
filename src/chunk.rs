use std::{
    borrow::Cow,
    collections::HashMap,
    mem::size_of,
    path::{Path, PathBuf},
    sync::Arc,
};

use miette::NamedSource;

use crate::{
    bytecode::{Bytecode, JumpToUndecidedAddress},
    compiler::ExpressionResultMode,
    token::Span,
    value::LuaString,
    vm::JumpAddr,
};

#[derive(Debug, Clone)]
pub(crate) struct ChunkUpValue {
    pub(crate) is_local: bool,
    pub(crate) index: u8,
    pub(crate) name: Option<LuaString>,
}

#[derive(Debug, Clone)]
pub struct Chunk<'source> {
    pub(crate) name: LuaString,
    // TODO: Maybe this can be a Cow<'source, Path>?
    pub(crate) filename: Option<PathBuf>,
    pub(crate) source: Arc<Cow<'source, [u8]>>,
    pub(crate) instructions: Vec<u8>,
    pub(crate) instruction_spans: HashMap<usize, Span>,
    pub(crate) max_registers: u8,
    pub(crate) num_params: u8,
    pub(crate) has_varargs: bool,
    pub(crate) upvalues: Vec<ChunkUpValue>,
}

impl<'source> Chunk<'source> {
    pub(crate) fn new(
        name: LuaString,
        filename: Option<PathBuf>,
        source: Arc<Cow<'source, [u8]>>,
        num_params: u8,
        has_varargs: bool,
        upvalues: Vec<ChunkUpValue>,
    ) -> Self {
        Self {
            name,
            filename,
            source,
            instructions: vec![],
            instruction_spans: HashMap::new(),
            max_registers: 0,
            num_params,
            has_varargs,
            upvalues,
        }
    }

    pub fn get_filename(&self) -> Option<&Path> {
        self.filename.as_deref()
    }

    pub fn get_name(&self) -> &[u8] {
        &self.name
    }

    pub fn get_source(&self) -> &[u8] {
        &self.source
    }

    pub fn get_current_addr(&self) -> JumpAddr {
        self.instructions.len() as JumpAddr
    }

    pub(crate) fn push_bytecode(&mut self, bytecode: Bytecode, span: Option<Span>) {
        let instruction_index = self.instructions.len();
        self.instructions.extend_from_slice(&bytecode.into_bytes());
        if let Some(span) = span {
            self.instruction_spans.insert(instruction_index, span);
        }
    }

    #[must_use]
    pub(crate) fn push_undecided_jump(
        &mut self,
        jump: JumpToUndecidedAddress,
        span: Option<Span>,
    ) -> JumpAddr {
        let instruction_index = self.instructions.len();
        let jump_addr_ip =
            (instruction_index + jump.instruction_size_without_address()) as JumpAddr;

        self.push_bytecode(jump.into_bytecode(jump_addr_ip), span);

        jump_addr_ip
    }

    pub(crate) fn push_call_instruction(
        &mut self,
        function_register: u8,
        args_multres: bool,
        result_mode: ExpressionResultMode,
        num_args: u8,
        span: Option<Span>,
    ) {
        // Instruction: [CALL|CALLM] [NUM_RESULTS] [NUM_KNOWN_ARGUMENTS]
        // CALL for regular calls, CALLM for calls with a multres expression at
        // the end of its arguments.
        // NUM_RESULTS is 1 for single result, 0 for multres.
        // NUM_KNOWN_ARGUMENTS is the number of known arguments, not counting
        // the multres at the end (if any), plus 1.
        self.push_bytecode(
            if args_multres {
                Bytecode::CallM {
                    func_register: function_register,
                    result_mode,
                    num_args,
                }
            } else {
                Bytecode::Call {
                    func_register: function_register,
                    result_mode,
                    num_args,
                }
            },
            span,
        );
    }

    pub(crate) fn push_callt_instruction(
        &mut self,
        args_from_register: u8,
        args_multres: bool,
        result_mode: ExpressionResultMode,
        num_args: u8,
        span: Option<Span>,
    ) {
        // Instruction: [CALL|CALLM] [NUM_RESULTS] [NUM_KNOWN_ARGUMENTS]
        // CALL for regular calls, CALLM for calls with a multres expression at
        // the end of its arguments.
        // NUM_RESULTS is 1 for single result, 0 for multres.
        // NUM_KNOWN_ARGUMENTS is the number of known arguments, not counting
        // the multres at the end (if any), plus 1.
        self.push_bytecode(
            if args_multres {
                Bytecode::CallTM {
                    args_from_register,
                    result_mode,
                    num_args,
                }
            } else {
                Bytecode::CallT {
                    args_from_register,
                    result_mode,
                    num_args,
                }
            },
            span,
        );
    }

    pub fn patch_addr_placeholder(&mut self, index: JumpAddr) {
        let addr = self.instructions.len() as JumpAddr;
        self.patch_addr_placeholder_with(index, addr);
    }

    pub fn patch_addr_placeholder_with(&mut self, index: JumpAddr, addr: JumpAddr) {
        let addr_bytes = addr.to_be_bytes();
        self.instructions[index as usize..index as usize + size_of::<JumpAddr>()]
            .copy_from_slice(&addr_bytes);
    }

    pub fn get_instructions(&self) -> &[u8] {
        &self.instructions
    }

    pub fn named_source(&self) -> NamedSource<Vec<u8>> {
        NamedSource::new(String::from_utf8_lossy(&self.name), self.source.to_vec())
            .with_language("lua")
    }
}
