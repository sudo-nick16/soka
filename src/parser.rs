use crate::ast::{
    Block,
    Expr::{self, AssignExpr, BinaryExpr, CallExpr, IdentLiteral, NumberLiteral},
    Stmt::{self, ExprStmt, FnStmt, ForStmt, LetStmt},
};
use crate::token::{Kind, Token};

#[derive(Debug, Clone, Default)]
pub struct Parser {
    tokens: Box<Vec<Token>>,
    index: usize,
}

fn get_precedence(token: &Token) -> i64 {
    if !token.is_binary_op() {
        return -1;
    }
    match token.kind {
        Kind::Assign | Kind::MulAssign | Kind::DivAssign | Kind::MinusAssign | Kind::PlusAssign => {
            0
        }
        Kind::Lt | Kind::Lte | Kind::Gt | Kind::Gte | Kind::And | Kind::Or => 0,
        Kind::Plus | Kind::Minus => 1,
        Kind::Slash | Kind::Asterisk => 2,
        Kind::Eq | Kind::NotEq => 4,
        _ => -1,
    }
}

fn get_assosciativity(token: &Token) -> i64 {
    if !token.is_binary_op() {
        return -1;
    }
    match token.kind {
        Kind::Plus => 0,
        Kind::Minus => 1,
        Kind::Asterisk => 0,
        Kind::Slash => 1,
        Kind::Assign | Kind::MulAssign | Kind::DivAssign | Kind::MinusAssign | Kind::PlusAssign => {
            0
        },
        Kind::Lt | Kind::Lte | Kind::Gt | Kind::Gte | Kind::And | Kind::Or => 0,
        _ => -1,
    }
}

impl Parser {
    pub fn new(tokens: Box<Vec<Token>>) -> Self {
        Self { tokens, index: 0 }
    }

    pub fn parse(&mut self) -> Block {
        let mut stmts: Vec<Stmt> = Vec::new();
        loop {
            let curr_token = match self.curr_token() {
                Some(tok) => tok,
                None => break,
            };
            match curr_token.kind {
                Kind::Import => {
                    println!("> parsing import statement");
                    self.advance();
                    match self
                        .curr_token()
                        .expect("Invalid import statement.")
                        .kind
                        .clone()
                    {
                        Kind::String(s) => {
                            self.advance();
                            if !self.curr_token_is(Kind::Semi) {
                                eprintln!("Error: Expected semicolon at {:?}", self.curr_token());
                                break;
                            }
                            self.advance();
                            stmts.push(Stmt::ImportStmt(s));
                        }
                        _ => {
                            eprintln!("Error: Invalid import statement.");
                            break;
                        }
                    }
                    if let Some(let_stmt) = self.parse_let_stmt() {
                        stmts.push(let_stmt);
                    } else {
                        break;
                    }
                }
                Kind::Let => {
                    println!("> parsing let statement");
                    if let Some(let_stmt) = self.parse_let_stmt() {
                        stmts.push(let_stmt);
                    } else {
                        break;
                    }
                }
                Kind::Fn => {
                    println!("> parsing function statement");
                    if let Some(stmt) = self.parse_function_stmt() {
                        stmts.push(stmt);
                    }
                }
                Kind::For => {
                    println!("> parsing for statement");
                    if let Some(stmt) = self.parse_for_stmt() {
                        stmts.push(stmt);
                    }
                }
                Kind::While => {
                    println!("> parsing while statement");
                    if let Some(stmt) = self.parse_while_stmt() {
                        stmts.push(stmt);
                    }
                }
                Kind::If => {
                    println!("> parsing if statement");
                    if let Some(stmt) = self.parse_if_else_stmt() {
                        stmts.push(stmt);
                    }
                }
                Kind::Ident(_) => match self.peek().expect("should be something").kind {
                    // Kind::MulAssign
                    // | Kind::DivAssign
                    // | Kind::MinusAssign
                    // | Kind::PlusAssign
                    // | Kind::Assign => {
                    //     println!("> parsing assign statement");
                    //     if let Some(ass_stmt) = self.parse_assign_stmt() {
                    //         stmts.push(ass_stmt);
                    //     }
                    // }
                    Kind::Lparen => {
                        println!("> parsing call expression statement");
                        if let Some(expr) = self.parse_expression(-1) {
                            if self.curr_token_is(Kind::Semi) {
                                self.advance();
                                stmts.push(ExprStmt(expr));
                            } else {
                                eprintln!("expected semicolon here");
                                break;
                            }
                        }
                    }
                    Kind::Lbrack => {
                        println!("> parsing member expression statement");
                        if let Some(expr) = self.parse_expression(-1) {
                            if self.curr_token_is(Kind::Semi) {
                                self.advance();
                                stmts.push(ExprStmt(expr));
                            } else {
                                eprintln!("expected semicolon here");
                                break;
                            }
                        }
                    }
                    _ => {
                        if let Some(expr) = self.parse_expression(-1) {
                            if self.curr_token_is(Kind::Semi) {
                                self.advance();
                                stmts.push(ExprStmt(expr));
                                continue;
                            }
                        }
                        eprintln!(
                            "Error: Statement not implemented - token {:?}",
                            self.peek().unwrap()
                        );
                        break;
                    }
                },
                Kind::Return => {
                    println!("> parsing return statement");
                    self.advance();
                    if self.curr_token_is(Kind::Semi) {
                        self.advance();
                        stmts.push(Stmt::ReturnStmt(Expr::NumberLiteral(0.0)));
                        continue;
                    }
                    if let Some(expr) = self.parse_expression(-1) {
                        if !self.curr_token_is(Kind::Semi) {
                            eprintln!("Error: Expected semicolon at {:?}", self.curr_token());
                        }
                        self.advance();
                        stmts.push(Stmt::ReturnStmt(expr));
                    }
                    eprintln!("Error: Parsing error at {:?}", self.curr_token());
                }
                Kind::Rbrace => {
                    return Block::new(stmts);
                }
                _ => {
                    println!("parsing expression as statement {:?}", curr_token);
                    if let Some(expr) = self.parse_expression(-1) {
                        if self.curr_token_is(Kind::Semi) {
                            self.advance();
                            stmts.push(ExprStmt(expr));
                        } else {
                            break;
                        }
                    } else {
                        break;
                    }
                }
            }
        }
        Block::new(stmts)
    }

    pub fn curr_token(&self) -> Option<&Token> {
        if self.index >= self.tokens.len() {
            return None;
        }
        self.tokens.get(self.index)
    }

    pub fn next_token(&mut self) -> Option<&Token> {
        if self.index + 1 >= self.tokens.len() {
            return None;
        }
        self.index += 1;
        self.tokens.get(self.index)
    }

    pub fn advance(&mut self) {
        self.index += 1;
    }

    pub fn peek(&self) -> Option<&Token> {
        self.peek_n(1)
    }

    pub fn peek_n(&self, n: usize) -> Option<&Token> {
        if self.index + n >= self.tokens.len() {
            return None;
        }
        self.tokens.get(self.index + n)
    }

    pub fn peek_is(&self, kind: Kind) -> bool {
        if let Some(pk) = self.peek() {
            if pk.kind == kind {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn curr_token_is(&self, kind: Kind) -> bool {
        if let Some(ct) = self.curr_token() {
            if ct.kind == kind {
                true
            } else {
                false
            }
        } else {
            false
        }
    }

    pub fn parse_let_stmt(&mut self) -> Option<Stmt> {
        // self.advance();
        // let var_ident = self.curr_token()?.clone().kind;
        // self.advance();
        // if !self.curr_token_is(Kind::Assign) {
        //     return None;
        // }
        // self.advance();
        // if let Some(expr) = self.parse_expression(-1) {
        //     if self.curr_token_is(Kind::Semi) {
        //         self.advance();
        //         Some(LetStmt(var_ident, expr))
        //     } else {
        //         None
        //     }
        // } else {
        //     None
        // }
        self.advance();
        let ass_expr = self
            .parse_expression(-1)
            .expect("Error: Expected assignment expression.");
        let left: Expr;
        let right: Expr;
        match ass_expr {
            AssignExpr(_, l, r) => {
                left = *l;
                right = *r;
            }
            _ => return None,
        }
        if !self.curr_token_is(Kind::Semi) {
            eprintln!("Error: Expected ; at line {}.", self.curr_token()?.line);
            return None;
        }
        self.advance();
        Some(LetStmt(left, right))
    }

    pub fn parse_assign_stmt(&mut self) -> Option<Stmt> {
        // let left = self.parse_expression(-1)?;
        // let op = self.curr_token_is(Kind::Assign)?;
        // if !op.is_assign_op() {
        //     return None;
        // }
        // self.advance();
        // let right = self.parse_expression(-1)?;
        // if !self.curr_token_is(Kind::Semi) {
        //     return None;
        // }
        // Some(ExprStmt(Expr::AssignExpr(
        //     op.kind,
        //     Box::new(left),
        //     Box::new(right),
        // )))
        None
    }

    pub fn parse_member_expr(&mut self) -> Option<Expr> {
        let mut var: Kind = self.curr_token().expect("has to be there").kind.clone();
        self.advance();
        if !self.curr_token_is(Kind::Lbrack) {
            return None;
        }
        self.advance();
        let member = self.parse_expression(-1)?;
        if !self.curr_token_is(Kind::Rbrack) {
            return None;
        }
        self.advance();
        Some(Expr::MemberExpr(var, Box::new(member)))
    }

    pub fn parse_if_else_stmt(&mut self) -> Option<Stmt> {
        let mut op: Kind = Kind::Eq;
        // if
        self.advance();
        // condition
        let condition = self.parse_expression(-1)?;
        println!("condition: {:?}", condition);
        match condition.clone() {
            BinaryExpr(bop, _, __) => op = bop,
            _ => {}
        }
        // {
        if !self.curr_token_is(Kind::Lbrace) {
            eprintln!("Error: Missing {{ at line {:?}", self.curr_token()?.line);
            None
        } else {
            self.advance();
            let if_block = self.parse();
            // }
            assert!(
                self.curr_token_is(Kind::Rbrace),
                "Missing }} at line {}",
                self.curr_token()?.line
            );
            self.advance();
            // else
            if self.curr_token_is(Kind::Else) {
                self.advance();
                // {
                assert!(
                    self.curr_token_is(Kind::Lbrace),
                    "Missing {{ at line {}",
                    self.curr_token()?.line
                );
                self.advance();
                let else_block = self.parse();
                // }
                assert!(
                    self.curr_token_is(Kind::Rbrace),
                    "Missing }} at line {}",
                    self.curr_token()?.line
                );
                self.advance();
                return Some(Stmt::IfElseStmt(op, condition, if_block, Some(else_block)));
            }
            Some(Stmt::IfElseStmt(op, condition, if_block, None))
        }
    }

    pub fn parse_while_stmt(&mut self) -> Option<Stmt> {
        println!("parsing while statement");
        self.advance();
        let cond = self.parse_expression(-1)?;
        if !self.curr_token_is(Kind::Lbrace) {
            eprintln!("Error: Missing {{ at line {}", self.curr_token()?.line);
        }
        self.advance();
        let block = self.parse();
        println!("while block: {:?}", block);
        if !self.curr_token_is(Kind::Rbrace) {
            eprintln!("Error: Missing {{ at line {}", self.curr_token()?.line);
        }
        self.advance();
        Some(Stmt::WhileStmt(cond, block))
    }

    pub fn parse_for_stmt(&mut self) -> Option<Stmt> {
        println!("parsing for statement");
        self.advance();
        let init = self.parse_expression(-1)?;
        if !self.curr_token_is(Kind::Semi) {
            eprintln!("Error: Missing ; at line {}", self.curr_token()?.line);
        }
        self.advance();
        let condition = self.parse_expression(-1)?;
        if !self.curr_token_is(Kind::Semi) {
            eprintln!("Error: Missing ; at line {}", self.curr_token()?.line);
        }
        self.advance();
        let step = self.parse_expression(-1)?;
        if !self.curr_token_is(Kind::Lbrace) {
            eprintln!("Error: Missing {{ at line {}", self.curr_token()?.line);
        }
        self.advance();
        let block = self.parse();
        if !self.curr_token_is(Kind::Rbrace) {
            eprintln!("Error: Missing {{ at line {}", self.curr_token()?.line);
        }
        self.advance();
        Some(ForStmt(init, condition, step, block))
    }

    pub fn parse_function_stmt(&mut self) -> Option<Stmt> {
        println!("parsing function statement");
        self.advance();
        let func_name = self.curr_token().expect("curr token has to exist").clone();
        let mut params: Vec<String> = Vec::new();
        assert!(self.peek_is(Kind::Lparen), "( missing");
        let mut is_first = true;

        self.advance();
        self.advance();

        while let Some(curr_tok) = self.curr_token() {
            match curr_tok.kind {
                Kind::Comma => {
                    if is_first {
                        return None;
                    }
                    self.advance();
                }
                Kind::Rparen => {
                    self.advance();
                    break;
                }
                Kind::Lbrace => {
                    assert!(0 != 0, "should not reach");
                    break;
                }
                _ => {
                    is_first = false;
                    if let Some(expr) = self.parse_primary_expr() {
                        match expr {
                            Expr::IdentLiteral(ident) => params.push(ident),
                            _ => {}
                        }
                    }
                }
            }
        }
        assert!(
            self.curr_token_is(Kind::Lbrace),
            "missing {{ invalid syntax"
        );
        self.advance();
        println!("params: {:?}", params);
        let block = self.parse();
        println!("block: {:?}", block);
        assert!(
            self.curr_token_is(Kind::Rbrace),
            "missing }} invalid syntax at line {}",
            self.curr_token()?.line
        );
        self.advance();
        Some(FnStmt(func_name.kind, params, block))
    }

    pub fn parse_call_expression(&mut self) -> Option<Expr> {
        println!("parsing call expression.");
        let mut args: Vec<Expr> = Vec::new();
        let callee = self.curr_token().expect("curr token has to exist").clone();

        self.advance();
        self.advance();

        let mut is_first = true;

        while let Some(curr_tok) = self.curr_token() {
            match curr_tok.kind {
                Kind::Comma => {
                    if is_first {
                        return None;
                    }
                    self.advance();
                }
                Kind::Rparen => {
                    self.advance();
                    break;
                }
                _ => {
                    is_first = false;
                    if let Some(expr) = self.parse_expression(-1) {
                        args.push(expr);
                    }
                }
            }
        }
        Some(CallExpr(callee.kind, args))
    }

    pub fn parse_expression(&mut self, min_prec: i64) -> Option<Expr> {
        let mut left = match self.parse_primary_expr() {
            Some(expr) => expr,
            None => {
                return None;
            }
        };
        let mut is_assign = false;
        while let Some(curr_tok) = self.clone().curr_token() {
            let prec = get_precedence(curr_tok);
            if !curr_tok.is_binary_op() || prec < min_prec {
                break;
            }
            if curr_tok.is_assign_op() {
                is_assign = true;
            }
            let next_min_prec = match get_assosciativity(curr_tok) {
                0 => prec,
                1 => prec + 1,
                _ => -1,
            };
            self.advance();
            if let Some(right) = self.parse_expression(next_min_prec + 1) {
                if !is_assign {
                    left = BinaryExpr(curr_tok.kind.clone(), Box::new(left), Box::new(right));
                } else {
                    left = AssignExpr(curr_tok.kind.clone(), Box::new(left), Box::new(right));
                }
            }
        }
        Some(left)
    }

    pub fn parse_primary_expr(&mut self) -> Option<Expr> {
        let curr = self.curr_token().expect("missing token");
        match curr.kind.clone() {
            Kind::Lparen => {
                self.advance();
                match self.parse_expression(-1) {
                    Some(left) => {
                        if self.curr_token_is(Kind::Rparen) {
                            self.advance();
                            Some(left)
                        } else {
                            None
                        }
                    }
                    None => None,
                }
            }
            Kind::Ident(name) => {
                if self.peek_is(Kind::Lparen) {
                    let expr = self.parse_call_expression();
                    expr
                } else if self.peek_is(Kind::Lbrack) {
                    let expr = self.parse_member_expr();
                    expr
                } else {
                    self.advance();
                    println!(
                        "found ident -- curr: {:?} , peek: {:?}",
                        self.curr_token(),
                        self.peek()
                    );
                    Some(IdentLiteral(name.clone()))
                }
            }
            Kind::Number(val) => {
                self.advance();
                Some(NumberLiteral(val.clone()))
            }
            Kind::String(val) => {
                self.advance();
                Some(Expr::StringLiteral(val.clone()))
            }
            Kind::Hex(val) => {
                self.advance();
                Some(Expr::HexLiteral(val.clone()))
            }
            Kind::True => {
                self.advance();
                Some(Expr::BoolLiteral(true))
            }
            Kind::False => {
                self.advance();
                Some(Expr::BoolLiteral(false))
            }
            Kind::Char(val) => {
                self.advance();
                Some(Expr::CharLiteral(val.clone()))
            }
            kind => {
                println!("reached invalid {:?} at line {}", kind, curr.line);
                assert!(false);
                None
            }
        }
    }
}
