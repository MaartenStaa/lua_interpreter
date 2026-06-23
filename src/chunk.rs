use std::{
    collections::HashMap,
    fmt::Debug,
    mem::size_of,
    path::{Path, PathBuf},
    sync::Arc,
};

use miette::NamedSource;

use crate::{
    bytecode::{Bytecode, JumpToUndecidedAddress},
    instruction::Instruction,
    token::Span,
    value::LuaString,
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
    pub(crate) instructions: Vec<u8>,
    pub(crate) instruction_spans: HashMap<usize, Span>,
    pub(crate) max_registers: u8,
    pub(crate) num_params: u8,
    pub(crate) has_varargs: bool,
    pub(crate) upvalues: Vec<ChunkUpValue>,
    pub(crate) local_vars: Vec<LocalVarInfo>,
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
        let opcode = self.instructions[setter_ip];

        if opcode == Instruction::GetUpval as u8 {
            // GetUpval: [opcode, dest_register, upval_index]
            let upval_index = self.instructions[setter_ip + 2] as usize;
            if let Some(name) = self
                .upvalues
                .get(upval_index)
                .and_then(|uv| uv.name.as_ref())
            {
                return Some(RegisterName::Upvalue(name));
            }
        } else if opcode == Instruction::Mov as u8 {
            // Mov: [opcode, dest_register, src_register]
            let src_register = self.instructions[setter_ip + 2];
            return self.get_register_name_inner(src_register, setter_ip, depth - 1);
        } else if opcode == Instruction::GetTableUpvalConstKey as u8 {
            // GetTableUpvalConstKey: [opcode, dest_register, upval_index, const_index(2)]
            let upval_index = self.instructions[setter_ip + 2] as usize;
            let key_const_index = ConstIndex::from_be_bytes([
                self.instructions[setter_ip + 3],
                self.instructions[setter_ip + 4],
            ]);
            if self
                .upvalues
                .get(upval_index)
                .and_then(|uv| uv.name.as_ref())
                .is_some_and(|n| &**n == b"_ENV")
            {
                return Some(RegisterName::Global { key_const_index });
            }

            return Some(RegisterName::Field { key_const_index });
        } else if opcode == Instruction::GetTableConstKey as u8 {
            // GetTableConstKey: [opcode, dest_register, table_register, const_index(2)]
            let key_const_index = ConstIndex::from_be_bytes([
                self.instructions[setter_ip + 3],
                self.instructions[setter_ip + 4],
            ]);

            return Some(RegisterName::Field { key_const_index });
        }

        None
    }

    fn find_register_setter(&self, register: u8, before_ip: usize) -> Option<usize> {
        let mut result = None;
        let mut ip = 0;
        while ip < before_ip {
            let instr = Instruction::from(self.instructions[ip]);
            if self.instructions.get(ip + 1).copied() == Some(register) {
                match instr {
                    Instruction::Mov
                    | Instruction::GetUpval
                    | Instruction::LoadConst
                    | Instruction::LoadClosure
                    | Instruction::LoadNil
                    | Instruction::LoadShort
                    | Instruction::LoadShortFloat
                    | Instruction::GetTable
                    | Instruction::GetTableConstKey
                    | Instruction::GetTableUpval
                    | Instruction::GetTableUpvalConstKey
                    | Instruction::LoadVararg => {
                        result = Some(ip);
                    }
                    _ => {}
                }
            }
            ip += instr.size();
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
