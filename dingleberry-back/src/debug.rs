use std::rc::Rc;

use crate::{byte_compiler::Function, bytecode::ByteCode, value::Value, vm::VM};

pub fn print_function_code(vm: &VM, function: &Rc<Function>) {
    use ByteCode::*;

    println!(
        "=== {} | {} bytes ===",
        function
            .identifier
            .as_ref()
            .and_then(|s| Some(s.as_str()))
            .unwrap_or_else(|| "Anonymous"),
        function.code.len(),
    );

    let mut idx = 0;
    while idx < function.code.len() {
        print!("{idx:0>6} | ");
        let instruction: ByteCode = function.code[idx].into();

        match instruction {
            ConstantByte => {
                byte_constant_instruction(&mut idx, "CONSTANT_BYTE", &function.code, &vm.constants)
            }
            ConstantShort => {
                constant_instruction(&mut idx, "CONSTANT_SHORT", &function.code, &vm.constants)
            }

            Pop => simple_instruction(&mut idx, "POP"),

            Add => simple_instruction(&mut idx, "ADD"),
            Sub => simple_instruction(&mut idx, "SUB"),
            Mul => simple_instruction(&mut idx, "MUL"),
            Div => simple_instruction(&mut idx, "DIV"),
            Negate => simple_instruction(&mut idx, "NEGATE"),

            Greater => simple_instruction(&mut idx, "GREATER"),
            GreaterEq => simple_instruction(&mut idx, "GREATER_EQ"),
            Less => simple_instruction(&mut idx, "LESS"),
            LessEq => simple_instruction(&mut idx, "LESS_EQ"),
            Equal => simple_instruction(&mut idx, "EQUAL"),
            NotEqual => simple_instruction(&mut idx, "NOT_EQUAL"),

            Or => simple_instruction(&mut idx, "OR"),
            And => simple_instruction(&mut idx, "AND"),

            Yield => simple_instruction(&mut idx, "YIELD"),
            Resume => simple_instruction(&mut idx, "RESUME"),
            WrapYielded => simple_instruction(&mut idx, "WRAP_YIELDED"),

            DefineGlobal => short_instruction(&mut idx, "DEFINE_GLOBAL", &function.code),

            GetLocal => byte_instruction(&mut idx, "GET_LOCAL", &function.code),
            SetLocal => byte_instruction(&mut idx, "SET_LOCAL", &function.code),
            GetGlobal => {
                constant_instruction(&mut idx, "GET_GLOBAL", &function.code, &vm.constants)
            }
            SetGlobal => {
                constant_instruction(&mut idx, "SET_GLOBAL", &function.code, &vm.constants)
            }

            IndexGet => simple_instruction(&mut idx, "INDEX_GET"),
            IndexSet => simple_instruction(&mut idx, "INDEX_SET"),

            This => simple_instruction(&mut idx, "THIS"),
            PropertyGet => {
                constant_instruction(&mut idx, "PROPERTY_GET", &function.code, &vm.constants)
            }
            PropertySet => {
                constant_instruction(&mut idx, "PROPERTY_SET", &function.code, &vm.constants)
            }

            Jump => short_instruction(&mut idx, "JUMP", &function.code),
            JumpNot => short_instruction(&mut idx, "JUMP_NOT", &function.code),

            Call => byte_instruction(&mut idx, "CALL", &function.code),
            Return => simple_instruction(&mut idx, "RETURN"),

            None => simple_instruction(&mut idx, "NONE"),
            IntoList => short_instruction(&mut idx, "INTO_LIST", &function.code),
            IntoTuple => short_instruction(&mut idx, "INTO_TUPLE", &function.code),

            _ => unreachable!("{instruction:?}"),
        }
    }
}

fn simple_instruction(idx: &mut usize, label: &'static str) {
    println!("{label}");
    *idx += 1;
}

fn byte_instruction(idx: &mut usize, label: &'static str, code: &Vec<u8>) {
    let b = code[*idx + 1];

    println!("{label}<{b}>");
    *idx += 2;
}

fn short_instruction(idx: &mut usize, label: &'static str, code: &Vec<u8>) {
    let l = code[*idx + 1];
    let r = code[*idx + 2];

    println!("{label}<{}>", u16::from_le_bytes([l, r]));
    *idx += 3;
}

fn byte_constant_instruction(
    idx: &mut usize,
    label: &'static str,
    code: &Vec<u8>,
    constants: &Vec<Value>,
) {
    let index = code[*idx + 1] as usize;

    println!("{label}<{index}:{}>", constants[index]);
    *idx += 2;
}

fn constant_instruction(
    idx: &mut usize,
    label: &'static str,
    code: &Vec<u8>,
    constants: &Vec<Value>,
) {
    let l = code[*idx + 1];
    let r = code[*idx + 2];

    let index = u16::from_le_bytes([l, r]) as usize;

    println!("{label}<{index}:{}>", constants[index]);
    *idx += 3;
}
