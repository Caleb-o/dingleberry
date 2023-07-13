use gc::Value;
use vm::VM;

pub mod byte_compiler;
pub mod bytecode;
pub mod gc;
pub mod vm;

pub fn nt_print(_: &mut VM, args: &[Value]) -> Value {
    for item in args {
        print!("{item}");
    }
    println!();

    Value::None
}
