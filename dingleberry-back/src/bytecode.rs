#[derive(Debug, Clone)]
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
    GetGlobal(String),
    SetGlobal(String),

    Jump(u16),
    JumpNot(u16),

    None,

    Call(u8),
    Return,

    IntoList(u8),
}
