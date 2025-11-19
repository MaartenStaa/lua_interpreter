use std::mem::size_of;

use crate::{
    error::RuntimeError,
    instruction::Instruction,
    macros::assert_function_const,
    value::{LuaConst, LuaFunctionDefinition, LuaVariableAttribute},
    vm::{Chunk, ConstIndex, JumpAddr, VM},
};

const IP_WIDTH: usize = 4;
const IP_HEX_WIDTH: usize = 2;
const IP_TEXT_WIDTH: usize = 16;

macro_rules! instr {
    ($name:expr) => {
        print!("{:01$}", $name, IP_TEXT_WIDTH);
    };
}

pub fn print_instructions(vm: &VM, chunk: &Chunk<'_>) {
    let instructions = chunk.get_instructions();
    let consts = vm.get_consts();
    let mut instruction_pointer = 0;

    loop {
        if instruction_pointer >= instructions.len() {
            break;
        }

        let instruction = instructions[instruction_pointer];
        print!(
            "{instruction_pointer:00$}    {instruction:01$x}  ",
            IP_WIDTH, IP_HEX_WIDTH
        );

        let instruction_increment = match Instruction::from(instruction) {
            // Stack manipulation
            Instruction::LoadConst => {
                let const_index_bytes = &instructions
                    [instruction_pointer + 1..instruction_pointer + 1 + size_of::<ConstIndex>()];
                let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
                instr!("LOAD_CONST");
                print_const(&consts[const_index as usize]);
                println!();
                1 + size_of::<ConstIndex>()
            }
            Instruction::LoadClosure => {
                let const_index_bytes = &instructions
                    [instruction_pointer + 1..instruction_pointer + 1 + size_of::<ConstIndex>()];
                let const_index = ConstIndex::from_be_bytes(const_index_bytes.try_into().unwrap());
                instr!("LOAD_CLOSURE");
                print_const(&consts[const_index as usize]);
                println!();

                let function = assert_function_const!(&consts[const_index as usize]);
                let extra_bytes = function.upvalues * 2;
                for i in 0..function.upvalues {
                    let ip = instruction_pointer + 1 + size_of::<ConstIndex>() + i * 2;
                    let is_local_byte = instructions[ip];
                    let local_index = instructions[ip + 1];
                    println!(
                        "{ip:04}        |               {:7} {}",
                        if is_local_byte == 0 {
                            "upvalue"
                        } else {
                            "local"
                        },
                        local_index
                    );
                }

                1 + size_of::<ConstIndex>() + extra_bytes
            }
            Instruction::Pop => {
                println!("POP");
                1
            }
            Instruction::Discard => {
                println!("DISCARD");
                1
            }
            Instruction::Swap => {
                let swap_offset = instructions[instruction_pointer + 1];
                instr!("SWAP");
                println!("{swap_offset}");
                2
            }
            Instruction::Align => {
                let align_offset = instructions[instruction_pointer + 1];
                instr!("ALIGN");
                println!("{align_offset}");
                2
            }
            Instruction::AlignVararg => {
                let align_offset = instructions[instruction_pointer + 1];
                instr!("ALIGN_VARARG");
                println!("{align_offset}");
                2
            }
            Instruction::DupFromMarker => {
                let offset = instructions[instruction_pointer + 1];
                instr!("DUP_FROM_MARK");
                println!("{offset}");
                2
            }

            // Binary operations
            // Arithmetic
            Instruction::Add => {
                println!("ADD");
                1
            }
            Instruction::Sub => {
                println!("SUB");
                1
            }
            Instruction::Mul => {
                println!("MUL");
                1
            }
            Instruction::Div => {
                println!("DIV");
                1
            }
            Instruction::Mod => {
                println!("MOD");
                1
            }
            Instruction::Pow => {
                println!("POW");
                1
            }
            Instruction::IDiv => {
                println!("IDIV");
                1
            }
            Instruction::Band => {
                println!("BAND");
                1
            }
            Instruction::Bor => {
                println!("BOR");
                1
            }
            Instruction::Bxor => {
                println!("BXOR");
                1
            }
            Instruction::Shl => {
                println!("SHL");
                1
            }
            Instruction::Shr => {
                println!("SHR");
                1
            }

            // Logical
            Instruction::And => {
                println!("AND");
                1
            }
            Instruction::Or => {
                println!("OR");
                1
            }

            // Comparison
            Instruction::Eq => {
                println!("EQ");
                1
            }
            Instruction::Ne => {
                println!("NE");
                1
            }
            Instruction::Lt => {
                println!("LT");
                1
            }
            Instruction::Le => {
                println!("LE");
                1
            }
            Instruction::Gt => {
                println!("GT");
                1
            }
            Instruction::Ge => {
                println!("GE");
                1
            }

            // Concatenation
            Instruction::Concat => {
                println!("CONCAT");
                1
            }

            // Unary operations
            Instruction::Neg => {
                println!("NEG");
                1
            }
            Instruction::Not => {
                println!("NOT");
                1
            }
            Instruction::Len => {
                println!("LEN");
                1
            }
            Instruction::BNot => {
                println!("BNOT");
                1
            }

            // Variables
            Instruction::SetGlobal => {
                let name_index_bytes = &instructions
                    [instruction_pointer + 1..instruction_pointer + 1 + size_of::<ConstIndex>()];
                let name_index = ConstIndex::from_be_bytes(name_index_bytes.try_into().unwrap());
                instr!("SET_GLOBAL");
                print_const(&consts[name_index as usize]);
                println!("   ({name_index})");
                1 + size_of::<ConstIndex>()
            }
            Instruction::GetGlobal => {
                let name_index_bytes = &instructions
                    [instruction_pointer + 1..instruction_pointer + 1 + size_of::<ConstIndex>()];
                let name_index = ConstIndex::from_be_bytes(name_index_bytes.try_into().unwrap());
                instr!("GET_GLOBAL");
                print_const(&consts[name_index as usize]);
                println!("   ({name_index})");
                1 + size_of::<ConstIndex>()
            }
            Instruction::SetLocal => {
                let local_index = instructions[instruction_pointer + 1];
                instr!("SET_LOCAL");
                println!("{local_index}");
                2
            }
            Instruction::GetLocal => {
                let local_index = instructions[instruction_pointer + 1];
                instr!("GET_LOCAL");
                println!("{local_index}");
                2
            }
            Instruction::SetLocalAttr => {
                let local_index = instructions[instruction_pointer + 1];
                let attr_value = instructions[instruction_pointer + 2];
                let attr = LuaVariableAttribute::try_from(attr_value).expect("valid attribute");
                instr!("SET_LOCAL_ATTR");
                println!("{local_index} {attr:?}");
                3
            }
            Instruction::SetUpval => {
                let upval_index = instructions[instruction_pointer + 1];
                instr!("SET_UPVAL");
                println!("{upval_index}");
                2
            }
            Instruction::GetUpval => {
                let upval_index = instructions[instruction_pointer + 1];
                instr!("GET_UPVAL");
                println!("{upval_index}");
                2
            }
            Instruction::LoadVararg => {
                let local_index = instructions[instruction_pointer + 1];
                let is_single_value = instructions[instruction_pointer + 2] == 1;
                instr!("LOAD_VARARG");
                println!(
                    "{local_index} {}",
                    if is_single_value { "single" } else { "multi" }
                );
                3
            }

            // Table
            Instruction::NewTable => {
                println!("NEW_TABLE");
                1
            }
            Instruction::SetTable => {
                println!("SET_TABLE");
                1
            }
            Instruction::GetTable => {
                println!("GET_TABLE");
                1
            }
            Instruction::AppendToTable => {
                println!("APPEND_TO_TABLE");
                1
            }

            // Function
            Instruction::Call => {
                let is_single_return = instructions[instruction_pointer + 1] == 1;
                instr!("CALL");
                println!("{}", if is_single_return { "single" } else { "multi" });
                2
            }
            Instruction::Return => {
                println!("RETURN");
                1
            }

            // Control
            Instruction::Jmp => {
                let offset_bytes = &instructions
                    [instruction_pointer + 1..instruction_pointer + 1 + size_of::<JumpAddr>()];
                let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
                instr!("JMP");
                println!("{offset:04}");
                1 + size_of::<JumpAddr>()
            }
            Instruction::JmpTrue => {
                let offset_bytes = &instructions
                    [instruction_pointer + 1..instruction_pointer + 1 + size_of::<JumpAddr>()];
                let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
                instr!("JMP_TRUE");
                println!("{offset:04}");
                1 + size_of::<JumpAddr>()
            }
            Instruction::JmpFalse => {
                let offset_bytes = &instructions
                    [instruction_pointer + 1..instruction_pointer + 1 + size_of::<JumpAddr>()];
                let offset = JumpAddr::from_be_bytes(offset_bytes.try_into().unwrap());
                instr!("JMP_FALSE");
                println!("{offset:04}");
                1 + size_of::<JumpAddr>()
            }

            // Other
            Instruction::Error => {
                let error_code = instructions[instruction_pointer + 1];
                let error = RuntimeError::try_from(error_code).expect("valid error code");
                instr!("ERROR");
                println!("{:?}", error);
                2
            }
        };
        instruction_pointer += instruction_increment;
    }
}

fn print_const(constant: &LuaConst) {
    match constant {
        LuaConst::Marker => print!("MARKER"),
        LuaConst::Nil => print!("NIL"),
        LuaConst::Boolean(b) => print!("{b}"),
        LuaConst::Number(n) => print_number(n),
        LuaConst::String(s) => print!("STRING    \"{}\"", String::from_utf8_lossy(s)),
        LuaConst::Function(LuaFunctionDefinition { name, ip, .. }) => {
            print!(
                "FUNCTION<{name}:{ip:04}>",
                name = String::from_utf8_lossy(name.as_deref().unwrap_or(b"[anonymous]")),
            )
        }
        LuaConst::Table(table) => print!("TABLE ({} fields)", table.keys().count()),
    }
}

fn print_number(number: &crate::value::LuaNumber) {
    match number {
        crate::value::LuaNumber::Integer(i) => print!("INT       {}", i),
        crate::value::LuaNumber::Float(f) => print!("FLOAT     {}", f),
    }
}
