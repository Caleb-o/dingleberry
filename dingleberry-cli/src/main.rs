use std::{env, fs, path::Path, rc::Rc};

use dingleberry_back::{byte_compiler::ByteCompiler, vm::VM};
use dingleberry_front::{parser::Parser, source::Source};

fn main() {
    if env::args().len() <= 1 {
        println!("Usage: dingleberry <script>");
        return;
    }

    let script_name = env::args().nth(1).unwrap();
    if !Path::exists(Path::new(&script_name)) {
        println!("Filepath does not exist '{script_name}'");
        return;
    }

    let content = fs::read_to_string(&script_name).unwrap();
    let source = Rc::new(Source::new(script_name, content));

    let mut parser = Parser::new(source.clone());
    let maybe_root = parser.run();

    if let Err(e) = maybe_root {
        println!("{e}");
        return;
    }

    let mut vm = VM::new();

    let root = maybe_root.unwrap();
    let compiler = ByteCompiler::new(&mut vm);

    let maybe_func = compiler.compile(root);
    if let Err(e) = maybe_func {
        println!("{e}");
        return;
    }

    let func = maybe_func.unwrap();
    if let Err(e) = vm.call(func) {
        println!("{e}");
    }
}
