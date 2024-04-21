#![allow(dead_code)]

use std::process::exit;

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

#[derive(Debug)]
enum Jmp {
    Abs(usize),
    Cond(JmpCond, usize),
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
    Memory,
    MemoryWrite,
    MemoryRead,

    Push(Val),
    Pop,
    Dup,
    Over,

    Add,
    Mod,

    Cast(CastType),
    Compare(CompareType),

    Jmp(Jmp),

    Dbg,
    Print,

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
                Jmp::Abs(offset) => write!(f, "jmp {offset}"),
                Jmp::Cond(JmpCond::False, offset) => write!(f, "jmpFalse {offset}"),
                Jmp::Cond(JmpCond::True, offset) => write!(f, "jmpTrue {offset}"),
            },
            Ops::Dbg => write!(f, "dbg"),
            Ops::Print => write!(f, "print"),
            Ops::Nop => write!(f, "nop"),
            Ops::Hlt => write!(f, "hlt"),
        }
    }
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum BacktraceType {
    If(usize, Option<usize>),
    While(usize, usize),
}

fn lex<'a>(filename: &str, tokenizer: Vec<Token<'a>>) -> Result<Vec<Ops>, ()> {
    let mut inst: Vec<Ops> = vec![];
    let mut last_while_inst: Option<usize> = None;
    let mut backtraces: Vec<BacktraceType> = vec![];
    let mut last_if_backtrace: Option<usize> = None;
    let mut hlt_exist = false;

    for token in tokenizer {
        let value = token.value;
        let loc = token.loc;

        match value {
            "+" => inst.push(Ops::Add),
            "%" => inst.push(Ops::Mod),
            "if" => {
                last_if_backtrace = Some(backtraces.len());
                backtraces.push(BacktraceType::If(inst.len(), None));
                inst.push(Ops::Nop);
            }
            "else" => {
                inst.push(Ops::Nop);
                let backtrace_id = last_if_backtrace.ok_or(()).map_err(|_| {})?;
                if let BacktraceType::If(if_inst, _else_inst) = backtraces[backtrace_id] {
                    backtraces[backtrace_id] = BacktraceType::If(if_inst, inst.len().into());
                }
                inst.push(Ops::Nop);
            }
            "end" => match backtraces
                .pop()
                .ok_or(0)
                .map_err(|_| leprintln!(filename, loc, "end need if/else/while"))?
            {
                BacktraceType::If(if_inst, None) => {
                    inst[if_inst] = Ops::Jmp(Jmp::Cond(JmpCond::False, inst.len()));
                }
                BacktraceType::If(if_inst, Some(else_inst)) => {
                    inst[if_inst] = Ops::Jmp(Jmp::Cond(JmpCond::False, else_inst + 1));
                    inst[else_inst] = Ops::Jmp(Jmp::Abs(inst.len()));
                }
                BacktraceType::While(while_addr, do_addr) => {
                    inst.push(Ops::Jmp(Jmp::Abs(while_addr)));
                    inst.push(Ops::Nop);
                    inst[do_addr] = Ops::Jmp(Jmp::Cond(JmpCond::False, inst.len()));
                }
            },
            "dbg" => inst.push(Ops::Dbg),
            "hlt" => {
                hlt_exist = true;
                inst.push(Ops::Hlt);
            }
            "true" => inst.push(Ops::Push(Val::Bool(true))),
            "false" => inst.push(Ops::Push(Val::Bool(false))),
            "=" => inst.push(Ops::Compare(CompareType::Eq)),
            "<" => inst.push(Ops::Compare(CompareType::Lt)),
            ">" => inst.push(Ops::Compare(CompareType::Gt)),
            "over" => inst.push(Ops::Over),
            "while" => {
                last_while_inst = Some(inst.len());
            }
            "do" => {
                let last_while_inst = last_while_inst
                    .ok_or(())
                    .map_err(|_| leprintln!(filename, loc, "do need while"))?;
                backtraces.push(BacktraceType::While(last_while_inst, inst.len()));
                inst.push(Ops::Nop);
            }
            "dup" => inst.push(Ops::Dup),
            "memory" => inst.push(Ops::Memory),
            "write" => inst.push(Ops::MemoryWrite),
            "read" => inst.push(Ops::MemoryRead),
            "pop" => inst.push(Ops::Pop),
            "print" => inst.push(Ops::Print),
            "char" => inst.push(Ops::Cast(CastType::Char)),
            "bool" => inst.push(Ops::Cast(CastType::Bool)),
            _ => {
                if value.starts_with("i") {
                    let i = value[1..].parse::<i32>().map_err(|_| {
                        leprintln!(filename, loc, "invalid integer, got: {}", value);
                        return ();
                    })?;
                    inst.push(Ops::Push(Val::Int(i)));
                } else {
                    leprintln!(filename, loc, "invalid token, got: {}", value);
                }
            }
        }
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
    fn in_bound_ip(&self) -> bool {
        let c = self.ip < self.inst.len();
        assert!(c, "IP out of bounds");
        c
    }
}

fn interpret(inst: Vec<Ops>) -> (VM, Vec<Val>) {
    let mut vm = VM::new(inst);

    let mut stack: Vec<Val> = vec![];
    let mut memory: Vec<Vec<i32>> = vec![];

    while vm.in_bound_ip() {
        match &vm.inst[vm.ip] {
            Ops::Dup => {
                let val = stack.last().expect("Stack underflow - Dup needs 1 value");
                let val = match val {
                    Val::Int(_) => val.clone(),
                    Val::Bool(_) => val.clone(),
                    Val::Char(_) => val.clone(),
                    Val::Addr(_) => todo!(),
                };
                stack.push(val);
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
            Ops::Push(val) => {
                stack.push(*val);
            }
            Ops::Over => {
                let v = stack
                    .get(stack.len() - 2)
                    .expect("Stack underflow - Over needs 2 values")
                    .clone();
                stack.push(v);
            }
            Ops::Dbg => {
                let v = stack.last();
                print!("{:?}", v);
                if let Some(Val::Addr(adrr)) = v {
                    print!(" {:?}", memory[*adrr]);
                }
                println!();
            }
            Ops::Jmp(Jmp::Abs(addr)) => {
                vm.goto(*addr);
                continue;
            }
            Ops::Jmp(Jmp::Cond(c, addr)) => {
                let v = stack
                    .pop()
                    .expect("Stack underflow - JmpCond needs 1 value");

                match (c, v) {
                    (JmpCond::True, Val::Bool(true)) | (JmpCond::False, Val::Bool(false)) => {
                        vm.goto(*addr);
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
            Ops::Pop => {
                stack.pop().expect("Stack underflow - Pop needs 1 value");
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
                if let Val::Addr(addr) = addr {
                    if let Val::Int(id) = id {
                        if let Val::Int(val) = val {
                            memory[*addr][id as usize] = val;
                        } else {
                            panic!("Invalid types for MemoryWrite");
                        }
                    } else {
                        panic!("Invalid types for MemoryWrite");
                    }
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
                if let Val::Addr(addr) = addr {
                    if let Val::Int(id) = id {
                        stack.push(Val::Int(memory[*addr][id as usize]));
                    } else {
                        panic!("Invalid types for MemoryWrite");
                    }
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
    (vm, stack)
}

#[allow(unreachable_patterns)]
fn start() -> Result<(), ()> {
    let path = "example/01.khe";
    let s = std::fs::read_to_string(path).map_err(|err| {
        eprintln!("Error: {:?}", err);
    })?;

    println!("------------");
    let mut tokenizer = Tokenizer::new(&s);
    let token = tokenizer.to_vec();
    token
        .iter()
        .for_each(|t| lprintln!(path, t.loc, "`{}`", t.value));

    println!("------------");
    let inst = lex(path, token)?;
    inst.iter()
        .enumerate()
        .for_each(|(i, s)| println!("{i:>2}: {s:?}"));

    println!("------------");
    let (vm, stack) = interpret(inst);

    println!("------------");
    println!("ip: {}", vm.ip);
    println!("stack: {:?}", stack);
    return Ok(());
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
