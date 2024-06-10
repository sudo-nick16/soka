use core::fmt;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Register {
    Rsp,
    Rbp,
    Rax,
    Rbx,
    Rcx,
    Rdx,
    Rdi,
    Rsi,
    R8,
    R9,
    R10,
    R11,
    R12,
    R13,
    R14,
    R15,
}

impl fmt::Display for Register {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Operand {
    Offset(Register, usize),
    Register(Register),
    Imm(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum AsmOp {
    OpMov(Operand, Operand),
    OpLea(Operand, Operand),
    OpAdd(Operand, Operand),
    OpSub(Operand, Operand),
    OpMul(Operand),
    OpDiv(Operand),
    OpPush(Operand),
    Label(usize),
    Directive(String),
}
