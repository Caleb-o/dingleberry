#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
#[repr(u8)]
pub enum ByteCode {
    ConstantByte,  // index - u8
    ConstantShort, // index - u16

    Pop,

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
        use ByteCode::*;
        match value {
            0 => ConstantByte,
            1 => ConstantShort,

            2 => Pop,

            3 => Add,
            4 => Sub,
            5 => Mul,
            6 => Div,
            7 => Negate,

            8 => Greater,
            9 => GreaterEq,
            10 => Less,
            11 => LessEq,
            12 => Equal,
            13 => NotEqual,

            14 => Or,
            15 => And,

            16 => Yield,
            17 => Resume,
            18 => WrapYielded,

            19 => DefineGlobal,

            20 => GetLocal,
            21 => SetLocal,
            22 => GetGlobal,
            23 => SetGlobal,

            24 => IndexGet,
            25 => IndexSet,

            26 => This,
            27 => Super,

            28 => PropertyGet,
            29 => PropertySet,

            30 => Jump,
            31 => JumpNot,

            32 => Call,
            33 => Return,

            34 => None,
            35 => IntoList,
            36 => IntoTuple,

            37 => Inherit,

            38 => CheckIterObj,
            39 => MakeIterObj,

            _ => unreachable!(),
        }
    }
}
