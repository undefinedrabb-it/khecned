#![allow(dead_code)]
#![feature(let_chains)]

use std::{collections::HashMap, process::exit};

#[macro_use]
pub mod tokenizer;
use crate::tokenizer::{Loc, Token, Tokenizer};

#[derive(Clone, Copy)]
enum Val {
    Int(i32),
    Bool(bool),
    Char(char),
    Addr(usize),
}
impl std::fmt::Debug for Val {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Val::Int(i) => write!(f, "i{i}"),
            Val::Bool(b) => write!(f, "{b}"),
            Val::Addr(a) => write!(f, "a{a}"),
            Val::Char(c) => write!(f, "'{c}'"),
        }
    }
}

#[derive(Debug)]
enum JmpCond {
    True,
    False,
}

struct Rel(i64);
impl Rel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        let Rel(i) = self;
        let sign = if *i == 0 {
            ' '
        } else if i.is_positive() {
            '+'
        } else {
            '-'
        };
        let i = i.abs();
        write!(f, "{sign}{i}")
    }
}
impl std::fmt::Debug for Rel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt(f)
    }
}
impl std::fmt::Display for Rel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.fmt(f)
    }
}

#[derive(Debug)]
enum Jmp {
    Rel(Rel),
    Cond(JmpCond, Rel),
}

#[derive(Debug)]
enum CastType {
    Bool,
    Char,
}

#[derive(Debug)]
enum CompareType {
    Eq,
    Ne,
    Gt,
    Ge,
    Lt,
    Le,
}

enum Ops {
    Push(Val),
    Pop,
    Dup,
    Over,

    Add,
    Mod,

    Cast(CastType),
    Compare(CompareType),

    Memory,
    MemoryWrite,
    MemoryRead,

    // Fn,
    FnCall(usize),
    FnRet,

    Dbg,
    Print,

    Jmp(Jmp),

    Nop,
    Hlt,
}
impl std::fmt::Debug for Ops {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Ops::Add => write!(f, "add"),
            Ops::Memory => write!(f, "memory"),
            Ops::Push(v) => write!(f, "push {v:?}"),
            Ops::MemoryWrite => write!(f, "memoryWrite"),
            Ops::MemoryRead => write!(f, "memoryRead"),
            Ops::Pop => write!(f, "pop"),
            Ops::Dup => write!(f, "dup"),
            Ops::Over => write!(f, "over"),
            Ops::Mod => write!(f, "mod"),
            Ops::Cast(c) => match c {
                CastType::Bool => write!(f, "cast bool"),
                CastType::Char => write!(f, "cast char"),
            },
            Ops::Compare(c) => match c {
                CompareType::Eq => write!(f, "eq"),
                CompareType::Ne => write!(f, "ne"),
                CompareType::Gt => write!(f, "gt"),
                CompareType::Ge => write!(f, "ge"),
                CompareType::Lt => write!(f, "lt"),
                CompareType::Le => write!(f, "le"),
            },
            Ops::Jmp(j) => match j {
                Jmp::Rel(offset) => write!(f, "jmp {offset}"),
                Jmp::Cond(JmpCond::False, offset) => write!(f, "jmpFalse {offset}"),
                Jmp::Cond(JmpCond::True, offset) => write!(f, "jmpTrue {offset}"),
            },
            Ops::FnCall(fn_inst) => write!(f, "call {fn_inst}"),
            Ops::FnRet => write!(f, "ret"),
            Ops::Dbg => write!(f, "dbg"),
            Ops::Print => write!(f, "print"),
            Ops::Nop => write!(f, "nop"),
            Ops::Hlt => write!(f, "hlt"),
        }
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
struct Ip(usize);

impl<T: Into<usize>> std::convert::From<T> for Ip {
    fn from(value: T) -> Self {
        Self(value.into())
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum EndBacktraceType {
    Fn(Ip),
    If(Ip, Option<Ip>),
    While(Ip, Ip),
}

const KEYWORD: &[&str] = &[
    "pop", "dup", "over", "+", "%", "if", "else", "while", "do", "fn", "end", "true", "false", "=",
    "<", ">", "memory", "write", "read", "char", "bool", "dbg", "print", "hlt",
];

struct LexConf {
    panic_on_first_error: bool,
}
fn lex_ops(value: &str, filename: &str, loc: Loc) -> Option<Ops> {
    match value {
        "pop" => Some(Ops::Pop),
        "dup" => Some(Ops::Dup),
        "over" => Some(Ops::Over),

        "+" => Some(Ops::Add),
        "%" => Some(Ops::Mod),

        "true" => Some(Ops::Push(Val::Bool(true))),
        "false" => Some(Ops::Push(Val::Bool(false))),

        "=" => Some(Ops::Compare(CompareType::Eq)),
        "<" => Some(Ops::Compare(CompareType::Lt)),
        ">" => Some(Ops::Compare(CompareType::Gt)),

        "memory" => Some(Ops::Memory),
        "write" => Some(Ops::MemoryWrite),
        "read" => Some(Ops::MemoryRead),

        "char" => Some(Ops::Cast(CastType::Char)),
        "bool" => Some(Ops::Cast(CastType::Bool)),

        "dbg" => Some(Ops::Dbg),
        "print" => Some(Ops::Print),

        _ => {
            if value.ends_with('i')
                && let Ok(ops) = value[..value.len() - 1].parse::<i32>().map_err(|_| {
                    // verify if it's needed
                    leprintln!(filename, loc, "invalid integer, got: {}", value);
                })
            {
                Some(Ops::Push(Val::Int(ops)))
            } else {
                None
            }
        }
    }
}

fn lex_control_flow<'a>(
    filename: &str,
    iter: impl Iterator<Item = &'a Token<'a>>,
    conf: &LexConf,
) -> Result<Vec<Ops>, ()> {
    let mut inst: Vec<Ops> = vec![];

    let mut end_backtraces: Vec<EndBacktraceType> = vec![];

    let mut last_while_inst: Option<usize> = None;
    let mut last_if_backtrace_id: Option<usize> = None;

    let mut fns: HashMap<&str, usize> = HashMap::new();
    let mut backtrack_patches: HashMap<&str, Vec<(usize, Loc)>> = HashMap::new();

    let mut hlt_exist = false;

    let mut iter = iter.peekable();
    while let Some(&Token { value, loc }) = iter.next() {
        // lprintln!(filename, loc, "{value}");

        if let Some(op) = lex_ops(value, filename, loc) {
            inst.push(op);
            continue;
        }

        match value {
            "if" => {
                last_if_backtrace_id = Some(end_backtraces.len());
                end_backtraces.push(EndBacktraceType::If(inst.len().into(), None));
                inst.push(Ops::Nop);
            }
            "else" => {
                inst.push(Ops::Nop);
                let backtrace_id = last_if_backtrace_id.ok_or(()).map_err(|_| {})?;
                if let EndBacktraceType::If(if_inst, _else_inst) = end_backtraces[backtrace_id] {
                    let elze = Some(inst.len().into());
                    end_backtraces[backtrace_id] = EndBacktraceType::If(if_inst, elze);
                }
                inst.push(Ops::Nop);
            }
            "while" => {
                last_while_inst = Some(inst.len());
            }
            "do" => {
                let last_while_inst = last_while_inst.ok_or(()).map_err(|_| {
                    leprintln!(filename, loc, "do need while");
                })?;
                end_backtraces.push(EndBacktraceType::While(
                    last_while_inst.into(),
                    inst.len().into(),
                ));
                inst.push(Ops::Nop);
            }
            "fn" => {
                // let p = *iter.peek().ok_or(())?;
                // TODO: add check if it used
                let name = iter.next().ok_or(()).map_err(|_| {
                    leprintln!(filename, loc, "function must have name");
                })?;
                if let Some(in_keyword) = iter.next()
                    && in_keyword.value != "in"
                {
                    leprintln!(filename, loc, "function must have `in` keyword");
                    return Err(());
                }
                fns.insert(name.value, inst.len() + 1);
                end_backtraces.push(EndBacktraceType::Fn(Ip(inst.len())));
                inst.push(Ops::Nop);
            }
            "end" => match end_backtraces
                .pop()
                .ok_or(())
                .map_err(|_| leprintln!(filename, loc, "end need if/else/while"))?
            {
                EndBacktraceType::If(Ip(if_inst), None) => {
                    let jmp_rel = inst.len() as i64 - if_inst as i64;
                    inst[if_inst] = Ops::Jmp(Jmp::Cond(JmpCond::False, Rel(jmp_rel)));
                }
                EndBacktraceType::If(Ip(if_inst), Some(Ip(else_inst))) => {
                    let jmp_to_else_rel = inst.len() as i64 - if_inst as i64;
                    inst[if_inst] = Ops::Jmp(Jmp::Cond(JmpCond::False, Rel(jmp_to_else_rel)));

                    let jmp_rel = inst.len() as i64 - else_inst as i64 - 2;
                    inst[else_inst] = Ops::Jmp(Jmp::Rel(Rel(jmp_rel)));
                }
                EndBacktraceType::While(Ip(while_addr), Ip(do_addr)) => {
                    let jmp_rel = while_addr as i64 - inst.len() as i64;
                    inst.push(Ops::Jmp(Jmp::Rel(Rel(jmp_rel))));
                    inst.push(Ops::Nop);
                    let jmp_do_addr = inst.len() as i64 - do_addr as i64;
                    inst[do_addr] = Ops::Jmp(Jmp::Cond(JmpCond::False, Rel(jmp_do_addr)));
                }
                EndBacktraceType::Fn(Ip(fn_inst)) => {
                    inst.push(Ops::FnRet);
                    let jmp_rel = inst.len() as i64 - fn_inst as i64;
                    inst[fn_inst] = Ops::Jmp(Jmp::Rel(Rel(jmp_rel)));
                }
            },

            "hlt" => {
                hlt_exist = true;
                inst.push(Ops::Hlt);
            }

            _ => {
                if let Some(backtrack_patch_list) = backtrack_patches.get_mut(value) {
                    backtrack_patch_list.push((inst.len(), loc));
                } else {
                    backtrack_patches.insert(value, vec![(inst.len(), loc)]);
                }
                inst.push(Ops::Nop);
                inst.push(Ops::Nop);
            }
        }
    }

    let mut any_fns_backtrack_error = false;
    for (name, fns_backtrack_insts) in backtrack_patches {
        let fn_inst = fns.get(name);
        for (fns_backtrack_inst, loc) in fns_backtrack_insts {
            if let Some(fn_inst) = fn_inst {
                inst[fns_backtrack_inst] = Ops::FnCall(*fn_inst);
            } else {
                if conf.panic_on_first_error {
                    leprintln!(filename, loc, "function not found: {}", name);
                    return Err(());
                } else {
                    any_fns_backtrack_error = true;
                }
                leprintln!(filename, loc, "invalid token, got: {}", name);
            }
        }
    }

    if any_fns_backtrack_error {
        return Err(());
    }

    if !hlt_exist {
        inst.push(Ops::Hlt)
    }

    Ok(inst)
}

struct VM {
    inst: Vec<Ops>,
    ip: usize,
}
impl VM {
    fn new(inst: Vec<Ops>) -> VM {
        VM { inst, ip: 0 }
    }
    fn goto(&mut self, addr: usize) {
        self.ip = addr;
    }
    fn goto_rel(&mut self, addr: i64) {
        self.ip = (self.ip as i64 + addr) as usize;
    }
    fn in_bound_ip(&self) -> bool {
        let c = self.ip < self.inst.len();
        assert!(c, "IP out of bounds");
        c
    }
}

fn interpret(inst: Vec<Ops>) -> Result<(VM, Vec<Val>), ()> {
    let mut vm = VM::new(inst);

    let mut stack: Vec<Val> = vec![];
    let mut memory: Vec<Vec<i32>> = vec![];

    let mut stacktrace: Vec<usize> = vec![];

    while vm.in_bound_ip() {
        // println!("ip {:?}: {:?}", vm.ip, vm.inst[vm.ip]);
        match &vm.inst[vm.ip] {
            Ops::Push(val) => {
                stack.push(*val);
            }
            Ops::Pop => {
                stack.pop().expect("Stack underflow - Pop needs 1 value");
            }
            Ops::Dup => {
                let val = stack.last().expect("Stack underflow - Dup needs 1 value");
                let val = match val {
                    Val::Int(_) => *val,
                    Val::Bool(_) => *val,
                    Val::Char(_) => *val,
                    Val::Addr(_) => todo!(),
                };
                stack.push(val);
            }
            Ops::Over => {
                let v = *stack
                    .get(stack.len() - 2)
                    .expect("Stack underflow - Over needs 2 values");
                stack.push(v);
            }

            Ops::Add => {
                let l = stack.pop().expect("Stack underflow - Add needs 2 values");
                let r = stack.pop().expect("Stack underflow - Add needs 2 values");
                match (l, r) {
                    (Val::Int(l), Val::Int(r)) => {
                        stack.push(Val::Int(l + r));
                    }
                    _ => {
                        panic!("Invalid types for Add");
                    }
                }
            }
            Ops::Mod => {
                let l = stack.pop().expect("Stack underflow - Mod needs 2 values");
                let r = stack.pop().expect("Stack underflow - Mod needs 2 values");
                match (l, r) {
                    (Val::Int(l), Val::Int(r)) => {
                        stack.push(Val::Int(l % r));
                    }
                    _ => {
                        panic!("Invalid types for Mod");
                    }
                }
            }

            Ops::FnCall(fn_inst) => {
                stacktrace.push(vm.ip + 1);
                vm.goto(*fn_inst);
                continue;
            }
            Ops::FnRet => {
                // println!("{:?}", stacktrace);
                let ret_to_inst = stacktrace.pop().ok_or(()).map_err(|_| {
                    eprintln!("Ip:{} - Stacktrace underflow ", vm.ip);
                })?;
                vm.goto(ret_to_inst + 1);
                continue;
            }

            Ops::Dbg => {
                let v = stack.last();
                print!("{:?}", v);
                if let Some(Val::Addr(adrr)) = v {
                    print!(" {:?}", memory[*adrr]);
                }
                println!();
            }
            Ops::Jmp(Jmp::Rel(addr)) => {
                vm.goto_rel(addr.0);
                continue;
            }
            Ops::Jmp(Jmp::Cond(c, addr)) => {
                let v = stack
                    .pop()
                    .expect("Stack underflow - JmpCond needs 1 value");

                match (c, v) {
                    (JmpCond::True, Val::Bool(true)) | (JmpCond::False, Val::Bool(false)) => {
                        vm.goto_rel(addr.0);
                        continue;
                    }
                    (_, Val::Bool(_)) => {
                        // Do nothing
                    }
                    _ => {
                        panic!("Invalid types for JmpCond");
                    }
                }
            }
            Ops::Cast(CastType::Bool) => {
                let v = stack.pop().expect("Stack underflow - Cast needs 1 value");
                // println!("{:?}", v);
                match v {
                    Val::Int(i) => {
                        stack.push(Val::Bool(i != 0));
                    }
                    Val::Bool(_) => {
                        // Do nothing
                    }
                    _ => {
                        panic!("Invalid types for Cast");
                    }
                }
            }
            Ops::Cast(CastType::Char) => {
                let v = stack.pop().expect("Stack underflow - Cast needs 1 value");

                match v {
                    Val::Int(i) => {
                        stack.push(Val::Char(char::from(i as u8)));
                    }
                    Val::Bool(_) => {
                        // Do nothing
                    }
                    _ => {
                        panic!("Invalid types for Cast");
                    }
                }
            }
            Ops::Compare(comp) => {
                let l = stack
                    .pop()
                    .expect("Stack underflow - Compare needs 2 values");
                let r = stack
                    .pop()
                    .expect("Stack underflow - Compare needs 2 values");
                match (comp, l, r) {
                    (CompareType::Eq, Val::Int(l), Val::Int(r)) => {
                        stack.push(Val::Bool(l == r));
                    }
                    (CompareType::Ne, Val::Int(l), Val::Int(r)) => {
                        stack.push(Val::Bool(l != r));
                    }
                    (CompareType::Gt, Val::Int(l), Val::Int(r)) => {
                        stack.push(Val::Bool(l > r));
                    }
                    (CompareType::Ge, Val::Int(l), Val::Int(r)) => {
                        stack.push(Val::Bool(l >= r));
                    }
                    (CompareType::Lt, Val::Int(l), Val::Int(r)) => {
                        stack.push(Val::Bool(l < r));
                    }

                    (CompareType::Le, Val::Int(l), Val::Int(r)) => {
                        stack.push(Val::Bool(l <= r));
                    }

                    _ => {
                        panic!("Invalid types for Compare");
                    }
                }
            }
            Ops::Memory => {
                let addr = stack
                    .pop()
                    .expect("Stack underflow - Memory needs 1 values");

                match addr {
                    Val::Int(size) => {
                        memory.push(vec![0; size as usize]);
                        stack.push(Val::Addr(memory.len() - 1));
                    }
                    _ => {
                        panic!("Invalid types for Memory");
                    }
                }
            }
            Ops::MemoryWrite => {
                let val = stack
                    .pop()
                    .expect("Stack underflow - MemoryWrite needs 1 value");
                let id = stack
                    .pop()
                    .expect("Stack underflow - MemoryWrite needs 2 values");
                let addr = stack
                    .last()
                    .expect("Stack underflow - MemoryWrite needs 3 values");
                if let Val::Addr(addr) = addr
                    && let Val::Int(id) = id
                    && let Val::Int(val) = val
                {
                    memory[*addr][id as usize] = val;
                } else {
                    panic!("Invalid types for MemoryWrite");
                }
            }
            Ops::MemoryRead => {
                let id = stack
                    .pop()
                    .expect("Stack underflow - MemoryWrite needs 1 values");
                let addr = stack
                    .last()
                    .expect("Stack underflow - MemoryWrite needs 2 values");

                if let Val::Addr(addr) = addr
                    && let Val::Int(id) = id
                {
                    stack.push(Val::Int(memory[*addr][id as usize]));
                } else {
                    panic!("Invalid types for MemoryWrite");
                }
            }
            Ops::Print => match stack.pop().expect("Stack underflow - Print needs 1 value") {
                Val::Int(i) => println!("{}", i),
                Val::Bool(b) => println!("{}", b),
                Val::Char(c) => println!("{}", c),
                _ => {
                    panic!("Invalid types for Print");
                }
            },
            Ops::Nop => {}
            Ops::Hlt => {
                break;
            }
        }

        vm.ip += 1;
    }
    Ok((vm, stack))
}

#[allow(unreachable_patterns)]
fn start() -> Result<(), ()> {
    let path = "example/03.khe";
    let s = std::fs::read_to_string(path).map_err(|err| {
        eprintln!("Error: {:?}", err);
    })?;

    // println!("------------");
    let mut tokenizer = Tokenizer::new(&s);
    let token = tokenizer.to_vec();
    // token
    //     .iter()
    //     .for_each(|t| lprintln!(path, t.loc, "`{}`", t.value));

    println!("------------");
    let inst = lex_control_flow(
        path,
        token.iter(),
        &LexConf {
            panic_on_first_error: false,
        },
    )?;
    inst.iter()
        .enumerate()
        .for_each(|(i, s)| println!("{i:>2}: {s:?}"));
    // return Err(());
    println!("------------");
    let (vm, stack) = interpret(inst)?;

    println!("------------");
    println!("ip: {}", vm.ip);
    println!("stack: {:?}", stack);
    Ok(())
}

fn main() {
    match start() {
        Ok(_) => exit(0),
        Err(_) => exit(1),
    }
}

// TODO: fix error messages on unwrap/expect etc.
// TODO: add if elif construct\
// TODO: create rule110
// TODO: more math ops,
// TODO: add binary shift and bianry ops & ^ etc.
// TODO: add logical ops && || etc.
// TODO: add type system and type check, betters than panic!
// TODO: add livetime to memory (or explicte free)
// TODO: try compile to asm
// do i need bootstrap/ interpreter?
// is it possible to compile it without bootstrap?
// or could i used llvm
// TODO: check if i need nop after while/do, else, is it possible to jump line after and execute it and check if i don't goto outside inst list
