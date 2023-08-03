#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ByteCode {
    ConstantByte(u8),
    ConstantShort(u16),

    Pop,
    PopN(u8),

    Add,
    Sub,
    Mul,
    Div,
    Negate,

    Greater,
    GreaterEq,
    Less,
    LessEq,
    Equal,
    NotEqual,

    Or,
    And,

    Yield,
    Resume,
    WrapYielded,

    DefineGlobal(u16),

    GetLocal(u8),
    SetLocal(u8),
    GetGlobal(u16),
    SetGlobal(u16),

    IndexGet,
    IndexSet,

    This,
    PropertyGet(u16),
    PropertySet(u16),

    Jump(u16),
    JumpNot(u16),

    Call(u8),
    Return,

    None,
    IntoList(u16),
    IntoTuple(u16),

    CheckIterObj(u16),
    MakeIterObj,

    Error,
}
