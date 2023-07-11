use crate::vm::VM;

#[derive(Debug, Clone, Copy)]
pub enum ByteCode {
    ConstantByte(u8),

    Pop,
    PopN(u8),

    Add,
    Sub,
    Mul,
    Div,

    GetLocal(u8),
    SetLocal(u8),
    GetGlobal(u8),
    SetGlobal(u8),

    Jump(u16),
    JumpNot(u16),

    Call(u8),
    Return,

    IntoList(u8),

    Halt,
}

impl ByteCode {
    pub fn execute(self, vm: &mut VM) -> Result<(), String> {
        Ok(())
    }

    #[inline]
    pub fn constant(index: u8, code: &mut Vec<ByteCode>) {
        code.push(Self::ConstantByte(index));
    }

    pub fn get_value(is_global: bool, index: u8, code: &mut Vec<ByteCode>) {
        code.push(if is_global {
            ByteCode::GetGlobal(index)
        } else {
            ByteCode::GetLocal(index)
        })
    }

    pub fn set_value(is_global: bool, index: u8, code: &mut Vec<ByteCode>) {
        code.push(if is_global {
            ByteCode::SetGlobal(index)
        } else {
            ByteCode::SetLocal(index)
        })
    }
}
