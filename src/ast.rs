use crate::token::{Kind, Type};

#[derive(Debug, Clone)]
pub struct Block {
    pub fn_count: usize,
    pub decl_size: usize,
    pub stmts: Vec<Stmt>,
}

impl Block {
    pub fn new(stmts: Vec<Stmt>) -> Self {
        let mut decl_size = 0;
        let mut fn_count = 0;
        for stmt in stmts.iter() {
            match stmt {
                Stmt::LetStmt(..) => decl_size += get_decl_size(&stmt),
                Stmt::FnStmt(..) => fn_count += 1,
                _ => {}
            }
        }
        Self {
            fn_count,
            decl_size,
            stmts,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Literal {
    Array(Type, usize),
    Int(f64),
    String(String),
    Boolean(bool),
}

#[derive(Debug, Clone)]
pub enum Stmt {
    ImportStmt(String),
    LetStmt(Expr, Expr),
    FnStmt(Kind, Vec<String>, Block),
    ForStmt(Expr, Expr, Expr, Block),
    WhileStmt(Expr, Block),
    ExprStmt(Expr),
    ReturnStmt(Expr),
    IfElseStmt(Kind, Expr, Block, Option<Block>),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    CallExpr(Kind, Vec<Expr>),
    BinaryExpr(Kind, Box<Expr>, Box<Expr>),
    NumberLiteral(f64),
    StringLiteral(String),
    HexLiteral(String),
    BoolLiteral(bool),
    CharLiteral(char),
    IdentLiteral(String),
    MemberExpr(Kind, Box<Expr>),
    AssignExpr(Kind, Box<Expr>, Box<Expr>),
    Unknown,
}

pub fn get_type(expr: &Expr) -> Type {
    match expr {
        Expr::BinaryExpr(..) => Type::Int,
        Expr::NumberLiteral(..) => Type::Int,
        Expr::StringLiteral(..) => Type::Str,
        _ => Type::UnKnown,
    }
}

impl Expr {
    pub fn get_int_literal(&self) -> Option<f64> {
        match self {
            Expr::NumberLiteral(n) => Some(*n),
            _ => None,
        }
    }
    pub fn get_ident_literal(&self) -> Option<String> {
        match self {
            Expr::IdentLiteral(s) => Some(s.clone()),
            _ => None,
        }
    }
}

pub fn get_decl_size(stmt: &Stmt) -> usize {
    match stmt {
        Stmt::LetStmt(ident, expr) => {
            let mut arr_n = 1;
            let mut type_size = 0;
            match ident {
                Expr::IdentLiteral(_) => {
                    type_size = 8;
                }
                Expr::MemberExpr(_, n) => {
                    arr_n = n
                        .get_int_literal()
                        .expect("Error: Array size has to be constant.")
                        as usize;
                    type_size = expr
                        .get_int_literal()
                        .expect("Error: Size of the element must be constant")
                        as usize;
                }
                _ => {
                    eprintln!("Error: Invalid identifier");
                }
            };
            return type_size * arr_n;
        }
        _ => {
            assert!(false, "Error: Invalid statement");
            0
        }
    }
}

pub fn get_type_size(expr: &Expr) -> usize {
    let mut type_size = 8;
    match expr {
        Expr::StringLiteral(_) => {
            type_size = 8;
        }
        Expr::NumberLiteral(_) => {
            type_size = 8;
        }
        Expr::IdentLiteral(_) => {
            type_size = 8;
        }
        Expr::MemberExpr(_, n) => {
            type_size =
                expr.get_int_literal()
                    .expect("Error: Size of the element must be constant") as usize;
        }
        _ => {
            eprintln!("Error: Invalid identifier");
            assert!(false, "Error: Invalid statement");
        }
    };
    return type_size;
}
