use std::{
    collections::HashMap,
    fmt::Debug,
    path::{Path, PathBuf},
    sync::Arc,
};

use miette::NamedSource;

use crate::{
    bytecode::{Bytecode, JumpToUndecidedAddress},
    instruction::{Instruction, OpCode},
    token::Span,
    value::{LuaConst, LuaString},
    vm::{ConstIndex, JumpAddr},
};

#[derive(Debug, Clone)]
pub(crate) struct ChunkUpValue {
    pub(crate) is_local: bool,
    pub(crate) index: u8,
    pub(crate) name: Option<LuaString>,
}

pub(crate) enum RegisterName<'a> {
    Local(&'a LuaString),
    Upvalue(&'a LuaString),
    Field { key_const_index: ConstIndex },
    Global { key_const_index: ConstIndex },
}

#[derive(Debug, Clone)]
pub(crate) struct LocalVarInfo {
    pub(crate) name: LuaString,
    pub(crate) register: u8,
    pub(crate) start_ip: usize,
    pub(crate) end_ip: usize,
}

#[derive(Clone)]
pub struct Chunk {
    pub(crate) name: LuaString,
    // TODO: Maybe this can be a Cow<'source, Path>?
    pub(crate) filename: Option<PathBuf>,
    pub(crate) source: Arc<Vec<u8>>,
    pub(crate) instructions: Vec<Instruction>,
    pub(crate) instruction_spans: HashMap<usize, Span>,
    pub(crate) max_registers: u8,
    pub(crate) num_params: u8,
    pub(crate) has_varargs: bool,
    pub(crate) upvalues: Vec<ChunkUpValue>,
    pub(crate) local_vars: Vec<LocalVarInfo>,
    pub(crate) consts: Vec<LuaConst>,
}

impl Chunk {
    pub(crate) fn new(
        name: LuaString,
        filename: Option<PathBuf>,
        source: Arc<Vec<u8>>,
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
            local_vars: vec![],
            consts: vec![],
        }
    }

    pub fn get_filename(&self) -> Option<&Path> {
        self.filename.as_deref()
    }

    pub fn get_name(&self) -> &LuaString {
        &self.name
    }

    pub fn get_source(&self) -> &[u8] {
        &self.source
    }

    pub fn register_const(&mut self, constant: LuaConst) -> ConstIndex {
        let index = self.consts.len();
        assert!(
            index < ConstIndex::MAX as usize,
            "too many constants registered (max {})",
            ConstIndex::MAX,
        );
        self.consts.push(constant);
        index as ConstIndex
    }

    pub fn lookup_const(&self, constant: &LuaConst) -> Option<ConstIndex> {
        self.consts
            .iter()
            .position(|c| c == constant)
            .map(|i| i as ConstIndex)
    }

    pub fn get_const(&self, index: ConstIndex) -> &LuaConst {
        &self.consts[index as usize]
    }

    pub fn get_current_addr(&self) -> JumpAddr {
        self.instructions.len() as JumpAddr
    }

    pub(crate) fn push_bytecode(&mut self, bytecode: Bytecode, span: Option<Span>) {
        let instruction_index = self.instructions.len();
        self.instructions.push(bytecode.into_bytes());
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
        let jump_addr_ip = instruction_index as JumpAddr;

        self.push_bytecode(jump.into_bytecode(JumpAddr::MAX), span);

        jump_addr_ip
    }

    pub fn patch_addr_placeholder(&mut self, index: JumpAddr) {
        let addr = self.instructions.len() as JumpAddr;
        self.patch_addr_placeholder_with(index, addr);
    }

    pub fn patch_addr_placeholder_with(&mut self, index: JumpAddr, addr: JumpAddr) {
        let instr = self
            .instructions
            .get_mut(index as usize)
            .expect("invalid jump address index");
        instr.set_bc(addr);
    }

    pub fn get_instructions(&self) -> &[Instruction] {
        &self.instructions
    }

    /// Get the name of the local variable stored in the given register, based on
    /// the given instruction pointer, if any.
    pub(crate) fn get_local_name(&self, register: u8, ip: usize) -> Option<&LuaString> {
        self.local_vars.iter().rev().find_map(|var| {
            if var.register == register && var.start_ip <= ip && ip < var.end_ip {
                Some(&var.name)
            } else {
                None
            }
        })
    }

    /// The the name of the value stored in the given register, at the time of the given
    /// instruction pointer, if any.
    pub(crate) fn get_register_name(&self, register: u8, ip: usize) -> Option<RegisterName<'_>> {
        // Recursion limit for following Mov chains (typically only 1 deep).
        self.get_register_name_inner(register, ip, 5)
    }

    fn get_register_name_inner(
        &self,
        register: u8,
        ip: usize,
        depth: u8,
    ) -> Option<RegisterName<'_>> {
        if depth == 0 {
            return None;
        }

        if let Some(name) = self.get_local_name(register, ip) {
            return Some(RegisterName::Local(name));
        }

        let setter_ip = self.find_register_setter(register, ip)?;
        let instruction = self.instructions[setter_ip];
        let opcode = instruction.opcode();

        if opcode == OpCode::GetUpval {
            // GetUpval: [opcode, dest_register, upval_index]
            let upval_index = instruction.bc() as usize;
            if let Some(name) = self
                .upvalues
                .get(upval_index)
                .and_then(|uv| uv.name.as_ref())
            {
                return Some(RegisterName::Upvalue(name));
            }
        } else if opcode == OpCode::Mov {
            // Mov: [opcode, dest_register, src_register]
            let src_register = instruction.b();
            return self.get_register_name_inner(src_register, setter_ip, depth - 1);
        } else if opcode == OpCode::GetTableUpvalConstKey {
            // GetTableUpvalConstKey: [opcode, dest_register, upval_index, const_index]
            let upval_index = instruction.b() as usize;
            let key_const_index = instruction.c() as ConstIndex;
            if self
                .upvalues
                .get(upval_index)
                .and_then(|uv| uv.name.as_ref())
                .is_some_and(|n| &**n == b"_ENV")
            {
                return Some(RegisterName::Global { key_const_index });
            }

            return Some(RegisterName::Field { key_const_index });
        } else if opcode == OpCode::GetTableConstKey {
            // GetTableConstKey: [opcode, dest_register, table_register, const_index]
            let key_const_index = instruction.c() as ConstIndex;

            return Some(RegisterName::Field { key_const_index });
        }

        None
    }

    fn find_register_setter(&self, register: u8, before_ip: usize) -> Option<usize> {
        let mut result = None;
        for (ip, instr) in self.instructions[..before_ip].iter().enumerate() {
            if instr.a() == register {
                match instr.opcode() {
                    OpCode::Mov
                    | OpCode::GetUpval
                    | OpCode::LoadConst
                    | OpCode::LoadClosure
                    | OpCode::LoadNil
                    | OpCode::LoadShort
                    | OpCode::LoadShortFloat
                    | OpCode::GetTable
                    | OpCode::GetTableConstKey
                    | OpCode::GetTableUpval
                    | OpCode::GetTableUpvalConstKey
                    | OpCode::LoadVararg => {
                        result = Some(ip);
                    }
                    _ => {}
                }
            }
        }
        result
    }

    pub fn named_source(&self) -> NamedSource<Vec<u8>> {
        NamedSource::new(String::from_utf8_lossy(&self.name), self.source.to_vec())
            .with_language("lua")
    }
}

impl Debug for Chunk {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Chunk")
            .field("name", &self.name)
            .field("filename", &self.filename)
            .field("source_length", &self.source.len())
            .field("instructions_length", &self.instructions.len())
            .field("max_registers", &self.max_registers)
            .field("num_params", &self.num_params)
            .field("has_varargs", &self.has_varargs)
            .field("upvalues", &self.upvalues)
            .finish()
    }
}
