use crate::ast::Block;
use crate::ast::Expr::{
    self, AssignExpr, BinaryExpr, CallExpr, CharLiteral, IdentLiteral, MemberExpr, NumberLiteral,
    StringLiteral,
};
use crate::lexer::strip_slashes;
use crate::token::Kind::{self, Asterisk, Eq, Gt, Gte, Lt, Lte, Minus, NotEq, Plus, Slash};
use std::collections::HashMap;
use std::io::Write;
use std::process::{Command, Stdio};
use std::{fs, usize};

use crate::ast::Stmt::{self, ExprStmt, IfElseStmt, LetStmt};

use crate::codegen::asm::Register::*;

use super::asm::Register;

#[derive(Debug)]
pub struct Codegen {
    instrs: Vec<String>,
    global_strs: Vec<(String, String)>,
    else_blocks: Vec<(Block, usize)>,
    if_stmt_count: usize,
    funcs: HashMap<String, bool>,
    label: usize,
}

pub fn gen_filename(dir: &str, filename: &String) -> String {
    let name = filename.split(".").nth(0).unwrap_or("my_code");
    format!("{}/{}", dir, name)
}

pub fn get_jmp_instr(k: Kind) -> String {
    match k {
        Kind::Eq => "jne".to_string(),
        Kind::NotEq => "je".to_string(),
        Kind::Lt => "jge".to_string(),
        Kind::Lte => "jg".to_string(),
        Kind::Gt => "jle".to_string(),
        Kind::Gte => "jl".to_string(),
        _ => "".to_string(),
    }
}

#[derive(Debug, Clone)]
pub struct Loc {
    reg: Register,
    offset: isize,
    size: usize,
}

impl Loc {
    pub fn new(reg: Register, offset: isize, size: usize) -> Self {
        Self { reg, offset, size }
    }
    pub fn offby(&mut self, offby: isize) {
        self.offset += offby;
    }
    pub fn to_string(&self) -> String {
        if self.offset < 0 {
            format!("{}{}", self.reg.to_string().to_lowercase(), self.offset)
        } else {
            format!("{}+{}", self.reg.to_string().to_lowercase(), self.offset)
        }
    }
}

#[derive(Debug)]
pub struct Env {
    vars: HashMap<String, (Loc, Expr, usize)>,
    stack_off: isize,
}

impl Env {
    pub fn new() -> Self {
        Self {
            vars: HashMap::new(),
            stack_off: 0,
        }
    }
}

const REGISTERS: [&str; 7] = ["rax", "rdi", "rsi", "rdx", "r10", "r8", "r9"];

impl Codegen {
    pub fn new() -> Self {
        Self {
            instrs: Vec::new(),
            global_strs: Vec::new(),
            else_blocks: Vec::new(),
            if_stmt_count: 0,
            funcs: HashMap::new(),
            label: 0,
        }
    }

    pub fn get_label(&mut self) -> usize {
        self.label += 1;
        self.label
    }

    pub fn prepend_header(&mut self) {
        self.instrs.push(String::from(".intel_syntax noprefix\n"));
        self.instrs.push(String::from("num_len = 22\n"));
        self.instrs.push(String::from("print_num:\n"));
        self.instrs.push(String::from("push rbp\n"));
        self.instrs.push(String::from("mov rbp, rsp\n"));
        self.instrs.push(String::from("sub rsp, num_len\n"));
        self.instrs.push(String::from("mov r9, num_len\n"));
        self.instrs.push(String::from("dec r9\n"));
        self.instrs
            .push(String::from("mov byte ptr [rsp+r9], 10\n"));
        self.instrs.push(String::from("dec r9\n"));

        self.instrs.push(String::from("cmp rdi, 0\n"));
        self.instrs.push(String::from("jne L0\n"));
        self.instrs
            .push(String::from("mov byte ptr [rsp+r9], 48\n"));
        self.instrs.push(String::from("dec r9\n"));

        self.instrs.push(String::from("L0:\n"));
        self.instrs.push(String::from("cmp rdi, 0\n"));
        self.instrs.push(String::from("je L1\n"));
        self.instrs.push(String::from("mov rax, rdi\n"));
        self.instrs.push(String::from("mov rcx, 10\n"));
        self.instrs.push(String::from("cqo\n"));
        self.instrs.push(String::from("div rcx\n"));
        self.instrs.push(String::from("mov rdi, rax\n"));
        self.instrs.push(String::from("add edx, 48\n"));
        self.instrs
            .push(String::from("mov byte ptr [rsp+r9], dl\n"));
        self.instrs.push(String::from("dec r9\n"));
        self.instrs.push(String::from("jmp L0\n"));
        self.instrs.push(String::from("L1:\n"));
        self.instrs.push(String::from("inc r9\n"));
        self.instrs.push(String::from("mov rax, 1\n"));
        self.instrs.push(String::from("mov rdi, 1\n"));
        self.instrs.push(String::from("lea rsi, [rsp+r9]\n"));
        self.instrs.push(String::from("mov r8, num_len\n"));
        self.instrs.push(String::from("sub r8, r9\n"));
        self.instrs.push(String::from("mov rdx, r8\n"));
        self.instrs.push(String::from("syscall\n"));
        self.instrs.push(String::from("mov rsp, rbp\n"));
        self.instrs.push(String::from("pop rbp\n"));
        self.instrs.push(String::from("ret\n"));
        self.instrs.push(String::from(".global _start\n"));
    }

    pub fn append_footer(&mut self, env: &mut Env, exit_code: i64) {
        self.instrs.push(String::from("_start:\n"));
        self.instrs.push(String::from("call main\n"));
        self.instrs.push(String::from("\n"));
        self.instrs.push(format!(
            "EXIT:\nmov rax, 60\nmov rdi, {}\nsyscall\n",
            exit_code
        ));
        for (b, ip) in self.else_blocks.clone().iter() {
            self.instrs.push(format!("\n.E{}:\n", ip));
            self.gen_code(env, &b);
            self.instrs.push(format!("jmp .CE{}\n", ip));
        }
        self.instrs.push(String::from("\n.data\n"));
        for s in self.global_strs.iter() {
            self.instrs
                .push(format!("{}:\n\t.string \"{}\"\n", s.1, s.0));
        }
    }

    pub fn gen_bin(&mut self, filename: String, ast: Block) {
        let mut env = Env::new();
        self.prepend_header();
        self.gen_code(&mut env, &ast);
        self.append_footer(&mut env, 0);
        let bin_name = gen_filename("bin", &filename);
        let asm_name = format!("{bin_name}.ns");
        let obj_name = format!("{bin_name}.o");
        println!("bin name: {}\nasm name: {}", bin_name, obj_name);
        let content = self.instrs.concat();
        let _ = fs::write(asm_name.clone(), content.as_bytes());
        let mut gas_cmd = Command::new("as")
            .stdin(Stdio::piped())
            .arg("-g")
            .arg("-Os")
            .arg("-o")
            .arg(obj_name.clone())
            .arg("--")
            .spawn()
            .expect("Failed to run as");
        let stdin = gas_cmd.stdin.as_mut().expect("Failed to open stdin");
        println!("Assembly: \n{}", content);
        stdin.write(content.as_bytes());
        gas_cmd.wait_with_output().expect("Failed to wait for as");
        let mut ld_cmd = Command::new("ld")
            .arg("-o")
            .arg(bin_name)
            .arg("-e")
            .arg("_start")
            .arg(obj_name)
            .spawn()
            .expect("Failed to run ld cmd");
        ld_cmd.wait_with_output().expect("Failed to wait for ld");
    }

    pub fn gen_code(&mut self, env: &mut Env, ast: &Block) {
        for stmt in ast.stmts.clone() {
            match stmt {
                LetStmt(_, _) => {
                    self.gen_let_stmt(stmt, env);
                }
                ExprStmt(_) => {
                    self.gen_expr_stmt(stmt, env);
                }
                Stmt::FnStmt(..) => {
                    self.gen_fn_stmt(stmt, env);
                }
                Stmt::IfElseStmt(..) => {
                    self.gen_if_else_stmt(stmt, env);
                }
                Stmt::ReturnStmt(..) => {
                    self.gen_ret_stmt(stmt, env);
                }
                Stmt::ForStmt(..) => {
                    self.gen_for_stmt(stmt, env);
                }
                Stmt::WhileStmt(..) => {
                    self.gen_while_stmt(stmt, env);
                }
                Stmt::ImportStmt(..) => {
                    self.gen_import_stmt(stmt, env);
                }
                _ => {}
            }
        }
    }

    pub fn gen_import_stmt(&mut self, stmt: Stmt, env: &mut Env) {
        match stmt {
            Stmt::ImportStmt(lib) => {}
            _ => {}
        }
    }

    pub fn gen_ret_stmt(&mut self, stmt: Stmt, env: &mut Env) {
        match stmt {
            Stmt::ReturnStmt(expr) => {
                self.gen_expr(expr, env);
                self.instrs.push(String::from("mov rax, rsp\n"));
                self.instrs.push(String::from("mov rsp, rbp\n"));
                self.instrs.push(String::from("pop rbp\n"));
                self.instrs.push(String::from("ret\n"));
                self.instrs.push(String::from("\n"));
            }
            _ => {}
        }
    }

    pub fn gen_while_stmt(&mut self, stmt: Stmt, env: &mut Env) {
        match stmt {
            Stmt::WhileStmt(cond, block) => {
                let cond_label = self.get_label();
                let after_label = self.get_label();
                self.instrs.push(format!(".L{}:\n", cond_label));
                self.gen_expr(cond.clone(), env);
                let mut op: Kind = Kind::Invalid;
                match cond {
                    BinaryExpr(binop, ..) => {
                        op = binop;
                    }
                    _ => {
                        assert!(false, "Error: Invalid condition in while statement");
                    }
                }
                self.instrs
                    .push(format!("{} .L{}\n", get_jmp_instr(op), after_label));
                self.gen_code(env, &block);
                self.instrs.push(format!("jmp .L{}\n", cond_label));
                self.instrs.push(format!(".L{}:\n", after_label));
            }
            _ => {}
        }
    }

    pub fn gen_for_stmt(&mut self, stmt: Stmt, env: &mut Env) {
        match stmt {
            Stmt::ForStmt(init, cond, step, block) => {
                let cond_label = self.get_label();
                let after_label = self.get_label();
                let mut init_ident: String;
                match init {
                    AssignExpr(_, left, right) => {
                        init_ident = left.get_ident_literal().expect("Error: Invalid identifier");
                        env.stack_off -= 8;
                        let loc = Loc::new(Rbp, env.stack_off, 8);
                        env.vars
                            .insert(init_ident.clone(), (loc.clone(), *right.clone(), 8));
                        self.gen_expr(*right, env);
                        self.instrs.push(format!("pop r8\n"));
                        self.instrs.push(format!("mov [{}], r8\n", loc.to_string()));
                    }
                    _ => {
                        return;
                    }
                }
                self.instrs.push(format!(".L{}:\n", cond_label));
                self.gen_expr(cond.clone(), env);
                let mut op: Kind = Kind::Invalid;
                match cond {
                    BinaryExpr(binop, ..) => {
                        op = binop;
                    }
                    _ => {
                        assert!(false, "Error: Invalid condition in for statement");
                    }
                }
                self.instrs
                    .push(format!("{} .L{}\n", get_jmp_instr(op), after_label));
                self.gen_code(env, &block);
                self.gen_expr(step, env);
                self.instrs.push(format!("jmp .L{}\n", cond_label));
                self.instrs.push(format!(".L{}:\n", after_label));
                env.vars.remove(&init_ident);
                env.stack_off += 8;
            }
            _ => {}
        }
    }

    pub fn gen_if_else_stmt(&mut self, stmt: Stmt, env: &mut Env) {
        match stmt {
            IfElseStmt(op, condition, if_block, else_block) => {
                self.gen_expr(condition, env);
                let ec = self.if_stmt_count;
                self.if_stmt_count += 1;
                let ec = self.get_label();
                let mut has_else = false;
                if let Some(block) = else_block {
                    has_else = true;
                    self.else_blocks.push((block, ec));
                }
                match op {
                    Kind::Eq => {
                        if has_else {
                            self.instrs.push(format!("jne .E{}\n", ec));
                        } else {
                            self.instrs.push(format!("jne .CE{}\n", ec));
                        }
                    }
                    Kind::NotEq => {
                        if has_else {
                            self.instrs.push(format!("je .E{}\n", ec));
                        } else {
                            self.instrs.push(format!("je .CE{}\n", ec));
                        }
                    }
                    Kind::Lt => {
                        if has_else {
                            self.instrs.push(format!("jge .E{}\n", ec));
                        } else {
                            self.instrs.push(format!("jge .CE{}\n", ec));
                        }
                    }
                    Kind::Lte => {
                        if has_else {
                            self.instrs.push(format!("jg .E{}\n", ec));
                        } else {
                            self.instrs.push(format!("jg .CE{}\n", ec));
                        }
                    }
                    Kind::Gt => {
                        if has_else {
                            self.instrs.push(format!("jle .E{}\n", ec));
                        } else {
                            self.instrs.push(format!("jle .CE{}\n", ec));
                        }
                    }
                    Kind::Gte => {
                        if has_else {
                            self.instrs.push(format!("jl .E{}\n", ec));
                        } else {
                            self.instrs.push(format!("jl .CE{}\n", ec));
                        }
                    }
                    _ => {}
                }
                self.gen_code(env, &if_block);
                self.instrs.push(format!("\n.CE{}:\n", ec));
            }
            _ => {}
        }
    }

    pub fn gen_expr_stmt(&mut self, stmt: Stmt, env: &mut Env) {
        match stmt {
            ExprStmt(expr) => match expr {
                CallExpr(..) => {
                    self.gen_expr(expr, env);
                }
                AssignExpr(..) => {
                    self.gen_expr(expr, env);
                }
                _ => {}
            },
            _ => {}
        }
    }

    pub fn gen_let_stmt(&mut self, stmt: Stmt, env: &mut Env) {
        match stmt {
            Stmt::LetStmt(ident, expr) => {
                let mut arr_n = 1;
                let mut size = 0;
                let ident_name = match ident {
                    IdentLiteral(s) => s,
                    MemberExpr(s, n) => {
                        arr_n = n
                            .get_int_literal()
                            .expect("Error: Array size has to be constant.")
                            as isize;
                        assert!(arr_n > 0, "Error: Array size has to be greater than 0");
                        size = expr
                            .get_int_literal()
                            .expect("Error: Size of the element must be constant")
                            as usize;
                        s.literal()
                    }
                    _ => {
                        eprintln!("Error: Invalid identifier");
                        "".to_string()
                    }
                };
                if size != 0 {
                    env.stack_off -= size as isize * arr_n;
                    let loc = Loc::new(Rbp, env.stack_off, size * arr_n as usize);
                    env.vars
                        .insert(ident_name, (loc.clone(), expr.clone(), size));
                    return;
                }
                size = 8;
                env.stack_off -= size as isize * arr_n;
                // let reg = format!("rbp-{}", env.var_count * 8);
                let loc = Loc::new(Rbp, env.stack_off, size);
                env.vars
                    .insert(ident_name, (loc.clone(), expr.clone(), size));
                self.gen_expr(expr, env);
                self.instrs.push(format!("pop r8\n"));
                self.instrs.push(format!("mov [{}], r8\n", loc.to_string()));
            }
            _ => {
                eprintln!("Invalid statement");
            }
        }
    }

    pub fn gen_fn_stmt(&mut self, stmt: Stmt, _env: &mut Env) {
        match stmt {
            Stmt::FnStmt(fn_name, params, block) => {
                let mut env = Env::new();
                for param in params.iter() {
                    let off = (env.vars.len() + 2) * 8;
                    let loc = Loc::new(Rbp, off as isize, 8);
                    // Todo: Add type specific size while defininig params
                    env.vars.insert(param.to_string(), (loc, Expr::Unknown, 8));
                }
                for s in block.stmts.clone() {
                    match s {
                        Stmt::ReturnStmt(..) => {
                            self.funcs.insert(fn_name.literal(), true);
                        }
                        _ => {}
                    }
                }
                self.instrs.push(String::from("\n"));
                self.instrs.push(format!("{}:\n", fn_name.literal()));
                self.instrs.push(String::from("push rbp\n"));
                self.instrs.push(String::from("mov rbp, rsp\n"));
                self.instrs.push(format!("sub rsp, {}\n", block.decl_size));
                self.gen_code(&mut env, &block);
                self.instrs.push(String::from("mov rsp, rbp\n"));
                self.instrs.push(String::from("pop rbp\n"));
                self.instrs.push(String::from("ret\n"));
                self.instrs.push(String::from("\n"));
            }
            _ => {}
        }
    }

    pub fn gen_expr(&mut self, expr: Expr, env: &mut Env) {
        match expr {
            MemberExpr(ident, i) => {
                let var_info = env
                    .vars
                    .get(&ident.literal())
                    .expect("Undeclared variable. Please declare the variable.")
                    .clone();
                let loc = var_info.0;
                self.gen_expr(*i, env);
                self.instrs.push(format!("push {}\n", var_info.2));
                self.instrs.push(String::from("pop r8\n"));
                self.instrs.push(String::from("pop rax\n"));
                self.instrs.push(String::from("imul r8\n"));
                self.instrs.push(format!("mov rbx, rax\n"));
                self.instrs
                    .push(format!("pushq [{}+rbx]\n", loc.to_string()));
            }
            AssignExpr(op, left, right) => match *left {
                StringLiteral(_) => {}
                MemberExpr(var, i) => {
                    let var_info = env
                        .vars
                        .get(&var.literal())
                        .expect(
                            format!(
                                "Undeclared variable {:?}. Please declare the variable.",
                                var.literal()
                            )
                            .as_str(),
                        )
                        .clone();

                    let loc = var_info.0;
                    println!("assign member expr... {:?}", right);

                    self.gen_expr(*right, env);

                    self.gen_expr(*i, env);
                    if var_info.2 == 1 {
                        self.instrs.push(format!("pop rbx\n"));
                    } else {
                        self.instrs.push(format!("push {}\n", var_info.2));

                        self.instrs.push(String::from("pop r8\n"));
                        self.instrs.push(String::from("pop rax\n"));
                        self.instrs.push(String::from("imul r8\n"));

                        self.instrs.push(format!("mov rbx, rax\n"));
                    }

                    match op {
                        Kind::Assign => {
                            if var_info.2 == 1 {
                                self.instrs.push(String::from("pop rdx\n"));
                                self.instrs
                                    .push(format!("mov byte ptr [{}+rbx], dl\n", loc.to_string()));
                            } else {
                                self.instrs.push(format!("pop [{}+rbx]\n", loc.to_string()));
                            }
                        }
                        Kind::MulAssign => {
                            self.instrs
                                .push(format!("push [{}+rbx]\n", loc.to_string()));
                            self.instrs.push(String::from("pop rax\n"));
                            self.instrs.push(String::from("pop r8\n"));
                            self.instrs.push(String::from("imul r8\n"));
                            self.instrs
                                .push(format!("mov [{}+rbx], rax\n", loc.to_string()));
                        }
                        Kind::PlusAssign => {
                            self.instrs
                                .push(format!("push [{}+rbx]\n", loc.to_string()));
                            self.instrs.push(String::from("pop r8\n"));
                            self.instrs.push(String::from("pop r9\n"));
                            self.instrs.push(String::from("add r8, r9\n"));
                            self.instrs
                                .push(format!("mov [{}+rbx], r8\n", loc.to_string()));
                        }
                        Kind::DivAssign => {
                            self.instrs
                                .push(format!("push [{}+rbx]\n", loc.to_string()));
                            self.instrs.push(String::from("pop r8\n"));
                            self.instrs.push(String::from("pop r9\n"));
                            self.instrs.push(String::from("div r8, r9\n"));
                            self.instrs
                                .push(format!("mov [{}+rbx], r8\n", loc.to_string()));
                        }
                        Kind::MinusAssign => {
                            self.instrs
                                .push(format!("push [{}+rbx]\n", loc.to_string()));
                            self.instrs.push(String::from("pop r8\n"));
                            self.instrs.push(String::from("pop r9\n"));
                            self.instrs.push(String::from("sub r9, r8\n"));
                            self.instrs
                                .push(format!("mov [{}+rbx], r9\n", loc.to_string()));
                        }
                        _ => {}
                    }
                }
                IdentLiteral(var) => {
                    let loc = env
                        .vars
                        .get(&var)
                        .expect("Undeclared variables. Please declare the variable.")
                        .0
                        .clone();
                    self.gen_expr(*right, env);
                    match op {
                        Kind::Assign => {
                            self.instrs.push(format!("pop [{}]\n", loc.to_string()));
                        }
                        Kind::MulAssign => {
                            self.instrs.push(format!("push [{}]\n", loc.to_string()));
                            self.instrs.push(String::from("pop rax\n"));
                            self.instrs.push(String::from("pop r8\n"));
                            self.instrs.push(String::from("imul r8\n"));
                            self.instrs
                                .push(format!("mov [{}], rax\n", loc.to_string()));
                        }
                        Kind::PlusAssign => {
                            self.instrs.push(format!("push [{}]\n", loc.to_string()));
                            self.instrs.push(String::from("pop r8\n"));
                            self.instrs.push(String::from("pop r9\n"));
                            self.instrs.push(String::from("add r8, r9\n"));
                            self.instrs.push(format!("mov [{}], r8\n", loc.to_string()));
                        }
                        Kind::DivAssign => {
                            self.instrs.push(format!("push [{}]\n", loc.to_string()));
                            self.instrs.push(String::from("pop r8\n"));
                            self.instrs.push(String::from("pop r9\n"));
                            self.instrs.push(String::from("div r8, r9\n"));
                            self.instrs.push(format!("mov [{}], r8\n", loc.to_string()));
                        }
                        Kind::MinusAssign => {
                            self.instrs.push(format!("push [{}]\n", loc.to_string()));
                            self.instrs.push(String::from("pop r8\n"));
                            self.instrs.push(String::from("pop r9\n"));
                            self.instrs.push(String::from("sub r9, r8\n"));
                            self.instrs.push(format!("mov [{}], r9\n", loc.to_string()));
                        }
                        _ => {}
                    }
                }
                _ => {}
            },
            BinaryExpr(op, left, right) => {
                self.gen_expr(*left, env);
                self.gen_expr(*right, env);
                match op {
                    Plus => {
                        self.instrs.push(String::from("pop r8\n"));
                        self.instrs.push(String::from("pop r9\n"));
                        self.instrs.push(String::from("add r8, r9\n"));
                        self.instrs.push(String::from("pushq r8\n"));
                    }
                    Minus => {
                        self.instrs.push(String::from("pop r8\n"));
                        self.instrs.push(String::from("pop r9\n"));
                        self.instrs.push(String::from("sub r9, r8\n"));
                        self.instrs.push(String::from("pushq r9\n"));
                    }
                    Asterisk => {
                        self.instrs.push(String::from("pop rax\n"));
                        self.instrs.push(String::from("pop r8\n"));
                        self.instrs.push(String::from("imul r8\n"));
                        self.instrs.push(String::from("pushq rax\n"));
                    }
                    Slash => {
                        self.instrs.push(String::from("pop r8\n"));
                        self.instrs.push(String::from("pop r9\n"));
                        self.instrs.push(String::from("div r8, r9\n"));
                        self.instrs.push(String::from("pushq r8\n"));
                    }
                    Kind::Lsh => {
                        self.instrs.push(String::from("pop rcx\n"));
                        self.instrs.push(String::from("pop r9\n"));
                        self.instrs.push(String::from("shl r9, cl\n"));
                        self.instrs.push(String::from("pushq r9\n"));
                    }
                    Kind::Rsh => {
                        self.instrs.push(String::from("pop rcx\n"));
                        self.instrs.push(String::from("pop r9\n"));
                        self.instrs.push(String::from("shr r9, cl\n"));
                        self.instrs.push(String::from("pushq r9\n"));
                    }
                    Kind::Or => {
                        self.instrs.push(String::from("pop r8\n"));
                        self.instrs.push(String::from("pop r9\n"));
                        self.instrs.push(String::from("or r9, r8\n"));
                        self.instrs.push(String::from("pushq r9\n"));
                    }
                    Kind::And => {
                        self.instrs.push(String::from("pop r8\n"));
                        self.instrs.push(String::from("pop r9\n"));
                        self.instrs.push(String::from("and r9, r8\n"));
                        self.instrs.push(String::from("pushq r9\n"));
                    }
                    Lt | Lte | Gt | Gte | Eq | NotEq => {
                        self.instrs.push(String::from("pop r9\n"));
                        self.instrs.push(String::from("pop r8\n"));
                        self.instrs.push(String::from("cmp r8, r9\n"));
                    }
                    _ => {
                        eprintln!("Error: Invalid operator {}", op);
                    }
                }
            }
            NumberLiteral(num) => {
                self.instrs.push(format!("push {}\n", num));
            }
            IdentLiteral(ident) => {
                let var_info = env
                    .vars
                    .get(&ident)
                    .expect("Error: Undeclared variable")
                    .clone();
                if var_info.2 == 1 {
                    self.instrs.push(format!(
                        "mov r10, {}\n",
                        var_info.0.reg.to_string().to_lowercase()
                    ));
                    self.instrs
                        .push(format!("sub r10, {}\n", var_info.0.offset.abs()));
                    self.instrs.push(format!("push r10\n"));
                } else {
                    self.instrs
                        .push(format!("push [{}]\n", var_info.0.to_string()));
                }
            }
            Expr::BoolLiteral(b) => {
                self.instrs.push(format!("push {}\n", b as usize));
            }
            StringLiteral(lit) => {
                let loc = format!(".LC{}", self.get_label());
                self.global_strs.push((lit.clone(), loc.clone()));
                self.instrs.push(format!("pushq offset {}\n", loc));
            }
            CharLiteral(lit) => {
                self.instrs.push(format!("push {}\n", lit as usize));
            }
            Expr::HexLiteral(hex) => {
                let num = usize::from_str_radix(hex.as_str(), 16).expect("Invalid hex literal");
                self.instrs.push(format!("push {}\n", num));
            }
            CallExpr(callee, args) => match callee.literal().as_str() {
                "print_num" => {
                    assert!(args.len() == 1);
                    self.gen_expr(args[0].clone(), env);
                    self.instrs.push(String::from("pop rdi\n"));
                    self.instrs.push(String::from("call print_num\n"));
                }
                "len" => {
                    assert!(args.len() == 1);
                    match args.first().unwrap() {
                        StringLiteral(s) => {
                            self.instrs.push(format!("pushq {}\n", s.len()));
                        }
                        IdentLiteral(ident) => {
                            let expr = env.vars.get(ident).unwrap().1.clone();
                            match expr {
                                StringLiteral(s) => {
                                    self.instrs
                                        .push(format!("pushq {}\n", strip_slashes(s).len()));
                                }
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }
                "print" => {
                    let n = args.len();
                    assert!(n == 1);
                    match args[0].clone() {
                        StringLiteral(s) => {
                            self.gen_expr(args[0].clone(), env);
                            self.instrs.push(String::from("pop rsi\n"));
                            self.instrs.push(format!("push {}\n", s.len()));
                        }
                        IdentLiteral(v) => {
                            let var_info = env
                                .vars
                                .get(&v)
                                .expect("Error: Undeclared variable")
                                .clone();
                            self.instrs
                                .push(format!("lea rsi, [{}]\n", var_info.0.to_string()));
                        }
                        // MemberExpr(var, off) => {
                        //     self.gen_expr(args[0].clone(), env);
                        //     self.instrs.push(String::from("pop rsi\n"));
                        // }
                        _ => {
                            assert!(false, "Error: Invalid argument to print function")
                        }
                    }
                    self.instrs.push(String::from("mov rax, 1\n"));
                    self.instrs.push(String::from("mov rdi, 1\n"));

                    self.instrs.push(String::from("pop rdx\n"));

                    self.instrs.push(String::from("syscall\n"));
                }
                "asm" => {
                    let n = args.len();
                    assert!(n == 1);
                    match args.first().unwrap() {
                        StringLiteral(s) => {
                            self.instrs.push(format!("{}\n", s.clone()));
                        }
                        _ => {}
                    }
                }
                "syscall" => {
                    let n = args.len();
                    assert!(n < 6);
                    for expr in args.iter() {
                        self.gen_expr(expr.clone(), env);
                    }
                    for i in 0..n {
                        self.instrs.push(format!("pop {}\n", REGISTERS[n - i - 1]));
                    }
                    self.instrs.push(String::from("syscall\n"));
                    self.instrs.push(String::from("push rax\n"));
                }
                _ => {
                    self.instrs.push(String::from("push rax\n"));
                    for arg in args.iter().rev() {
                        self.gen_expr(arg.clone(), env);
                    }
                    self.instrs.push(format!("call {}\n", callee.literal()));
                    for _ in 0..args.len() {
                        self.instrs.push(String::from("pop rbx\n"));
                    }
                    if let Some(has_return) = self.funcs.get(&callee.literal()) {
                        if *has_return {
                            self.instrs.push(String::from("pop rbx\n"));
                            self.instrs.push(format!("push [rax]\n"));
                            self.instrs.push(String::from("mov rax, rbx\n"));
                        }
                    } else {
                        self.instrs.push(String::from("pop rax\n"));
                    }
                    self.instrs.push(String::from("xor rbx, rbx\n"));
                }
            },
            _ => {}
        }
    }
}
