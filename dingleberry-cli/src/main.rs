use std::{
    fs::{self},
    path::Path,
    rc::Rc,
};

use clap::{
    clap_derive::{self},
    Parser,
};
use dingleberry_back::{byte_compiler::ByteCompiler, vm::VM};
use dingleberry_front::source::Source;
use dingleberry_shared::NativeModuleFlags;
use native_types::register_native_objects;
use serde_json::Value;

mod native_types;

const SCRIPT_TEMPLATE: &'static str = "module Program {
    fn main {
        // Print 'Hello, World!' to the terminal
        println('Hello, World!');
    }
}

Program.main();";

const DEFAULT_CONFIG: &'static str = r#"{
    "main": "src/main.dingle",
    "native_modules": []
}"#;

#[derive(clap_derive::Parser)] // requires `derive` feature
#[command(name = "dingleberry")]
#[command(bin_name = "dingleberry-cli")]
enum DingleCLI {
    #[clap(visible_alias = "i")]
    Init(InitArgs),
    #[clap(visible_alias = "r")]
    Run(RunArgs),
}

#[derive(clap::Args, Clone)]
#[command(author, version, about, long_about = None)]
pub struct InitArgs {
    pub project_name: String,
}

#[derive(clap::Args, Clone)]
#[command(author, version, about, long_about = None)]
pub struct RunArgs {
    pub file_path: String,
    #[clap(default_value_t = false, short = 'p', long)]
    pub project: bool,
    #[clap(default_value_t = false, short = 'c', long)]
    pub check: bool,
}

fn main() {
    match DingleCLI::parse() {
        DingleCLI::Init(InitArgs { project_name }) => try_init_project(project_name),
        DingleCLI::Run(args) => start_vm(args),
    }
}

fn try_init_project(project_name: String) {
    if Path::exists(&Path::new(&project_name)) {
        eprintln!("Project with name '{project_name}' already exists.");
        return;
    }

    fs::create_dir(&project_name).unwrap();
    fs::create_dir(&format!("{project_name}/src")).unwrap();

    fs::write(&format!("{project_name}/src/main.dingle"), SCRIPT_TEMPLATE).unwrap();
    fs::write(&format!("{project_name}/config.dconf"), DEFAULT_CONFIG).unwrap();

    println!("'{project_name}' created.");
}

fn start_vm(args: RunArgs) {
    let RunArgs {
        mut file_path,
        project,
        check,
    } = args;

    if !Path::exists(Path::new(&file_path)) {
        println!("Filepath does not exist '{file_path}'");
        return;
    }

    let mut native_flags = NativeModuleFlags::all_on();

    if project {
        // Reset
        native_flags = NativeModuleFlags::default();

        let config_path = format!("{file_path}/config.dconf");
        if !Path::exists(&Path::new(&config_path)) {
            eprintln!("Trying to run project '{file_path}' and could not find config.dconf");
            return;
        }

        let config_data = fs::read_to_string(config_path).unwrap();
        let json_data: Value = match serde_json::from_str(&config_data) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("Error: {e}");
                return;
            }
        };

        // Extract main point
        file_path = format!("{file_path}/{}", json_data["main"].as_str().unwrap());

        let flag_list = json_data["native_modules"].as_array().unwrap();
        for item in flag_list {
            if let Value::String(s) = item {
                match s.as_str() {
                    "File" | "file" => native_flags.file = true,
                    "Strings" | "strings" => native_flags.strings = true,
                    "List" | "list" => native_flags.list = true,
                    "Runtime" | "runtime" => native_flags.runtime = true,
                    "Coroutine" | "coroutine" => native_flags.coroutine = true,
                    _ => {}
                }
            }
        }

        // TODO: Pull native module values
    }

    let content = fs::read_to_string(&file_path).unwrap();
    let source = Rc::new(Source::new(file_path, content));

    let mut parser = dingleberry_front::parser::Parser::new(source.clone());
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

    if check {
        return;
    }

    register_native_objects(&mut vm, native_flags);

    if let Err(e) = vm.call(maybe_func.unwrap(), 0) {
        println!("Error: {e}");
        return;
    }

    vm.start();
}
