pub mod error;

#[derive(Debug, Clone, Copy, Default)]
pub struct NativeModuleFlags {
    pub file: bool,
    pub strings: bool,
    pub list: bool,
    pub runtime: bool,
    pub coroutine: bool,
}

impl NativeModuleFlags {
    pub fn all_on() -> Self {
        Self {
            file: true,
            strings: true,
            list: true,
            runtime: true,
            coroutine: true,
        }
    }
}
