#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum ByteCode {
    ConstantByte,  // index - u8
    ConstantShort, // index - u16

    Pop,
    PopN,

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

    DefineGlobal, // index - u16

    GetLocal,  // index - u8
    SetLocal,  // index - u8
    GetGlobal, // index - u16
    SetGlobal, // index - u16

    IndexGet,
    IndexSet,

    This,
    Super,

    PropertyGet, // index - u16
    PropertySet, // index - u16

    Jump,    // index - u16
    JumpNot, // index - u16

    Call, // arg_c - u8
    Return,

    None,
    IntoList,  // count - u16
    IntoTuple, // count - u16
    Inherit,

    CheckIterObj, // count - u16
    MakeIterObj,

    Error,
}

impl From<u8> for ByteCode {
    fn from(value: u8) -> Self {
        if value >= Self::Error as u8 {
            unreachable!();
        }
        unsafe { std::mem::transmute(value) }
    }
}
