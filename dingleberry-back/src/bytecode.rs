#[derive(Debug, Clone, Copy)]
pub enum ByteCode {
    ConstantByte(u8),
    Pop,
    PopN(u8),

    Add,
    Sub,
    Mul,
    Div,

    DefineGlobal(u8),

    GetLocal(u8),
    SetLocal(u8),
    GetGlobal(u8),
    SetGlobal(u8),

    Jump(u16),
    JumpNot(u16),

    Call(u8),
    Return,

    None,
    IntoList(u8),

    CheckIterObj(u16),
    MakeIterObj,
}
