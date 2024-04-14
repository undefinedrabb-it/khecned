#![allow(dead_code)]

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
            Val::Int(i) => write!(f, "i{}", i),
            Val::Bool(b) => write!(f, "{}", b),
            Val::Addr(a) => write!(f, "a{}", a),
            Val::Char(c) => write!(f, "'{}'", c),
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
    Abs(Option<usize>),
    Cond(JmpCond, Option<usize>),
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

#[derive(Debug)]
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

#[derive(PartialEq, Clone, Copy, Debug)]
enum BacktraceType {
    If(usize, Option<usize>),
    While(usize, Option<usize>),
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

#[allow(unreachable_patterns)]
fn main() {
    // simple if
    let s = "i34 i35 + i1 i1 = if  print end hlt";

    // simple while
    // let s = "
    // i65
    // while
    //     dup
    //     i65 i10 +
    //     >
    // do
    //     dup char print
    //     i1 +
    // end pop hlt";

    // simple while which prints even numbers
    // let s = "
    //     i65
    //     while
    //         dup
    //         65
    //         i65 i10 +
    //         >
    //     do
    //     i2 over mod bool if
    //             dup char print
    //         else
    //             dup print
    //     end
    //         i1 +
    //     end pop hlt";

    // {
    //     let mut i = 0;
    //     while i < 10 {
    //         println!("{}", i);
    //         i += 1;
    //     }
    // }

    // simple memory read/write
    // let s = "
    //     i20 memory dbg
    //     i2 i10 write
    //     i2 read
    //     print
    //     hlt";

    let mut inst: Vec<Ops> = vec![];
    let mut backtraces: Vec<BacktraceType> = vec![];
    let mut last_while_inst: Option<usize> = None;
    let mut last_if_backtrace: Option<usize> = None;

    for w in s.split_whitespace() {
        match w {
            "+" => inst.push(Ops::Add),
            "mod" => inst.push(Ops::Mod),
            "if" => {
                last_if_backtrace = Some(backtraces.len());
                backtraces.push(BacktraceType::If(inst.len(), None));
                inst.push(Ops::Nop);
            }
            "else" => {
                inst.push(Ops::Nop);
                if let Some(backtrace_id) = last_if_backtrace {
                    if let BacktraceType::If(if_inst, _else_inst) = backtraces[backtrace_id] {
                        backtraces[backtrace_id] = BacktraceType::If(if_inst, inst.len().into());
                    } else {
                        panic!("aaaaaaaaaaaa");
                    }
                }
                inst.push(Ops::Nop);
            }
            "end" => match backtraces.pop().expect("Backtrace empty") {
                BacktraceType::If(if_addr, else_addr) => {
                    if let Some(else_inst) = else_addr {
                        inst[if_addr] = Ops::Jmp(Jmp::Cond(JmpCond::False, (else_inst + 1).into()));
                        inst[else_inst] = Ops::Jmp(Jmp::Abs(inst.len().into()));
                    } else {
                        inst[if_addr] = Ops::Jmp(Jmp::Cond(JmpCond::False, inst.len().into()));
                    }
                }
                BacktraceType::While(while_addr, do_addr) => {
                    inst.push(Ops::Jmp(Jmp::Abs(while_addr.into())));
                    inst.push(Ops::Nop);
                    inst[do_addr.expect("Do address not found")] =
                        Ops::Jmp(Jmp::Cond(JmpCond::False, inst.len().into()));
                }
            },
            "dbg" => inst.push(Ops::Dbg),
            "hlt" => inst.push(Ops::Hlt),
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
                backtraces.push(BacktraceType::While(
                    last_while_inst.expect("except while "),
                    (inst.len()).into(),
                ));
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
                if w.starts_with("i") {
                    if let Ok(i) = w[1..].parse::<i32>() {
                        inst.push(Ops::Push(Val::Int(i)));
                    } else {
                        assert!(false, "Invalid word: {}", w);
                    }
                }
            }
        }
    }

    println!("{:?}", inst);
    let mut vm = VM::new(inst);

    let mut stack: Vec<Val> = vec![];
    let mut memory: Vec<Vec<i32>> = vec![];

    while vm.in_bound_ip() {
        match &vm.inst[vm.ip] {
            Ops::Dup => {
                let v = stack
                    .last()
                    .expect("Stack underflow - Dup needs 1 value")
                    .clone();
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
                        assert!(false, "Invalid types for Add");
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
                        assert!(false, "Invalid types for Mod");
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
                let addr = addr.expect("JmpAbs needs an address");
                vm.goto(addr);
                continue;
            }
            Ops::Jmp(Jmp::Cond(c, addr)) => {
                let v = stack
                    .pop()
                    .expect("Stack underflow - JmpCond needs 1 value");

                match (c, v) {
                    (JmpCond::True, Val::Bool(true)) | (JmpCond::False, Val::Bool(false)) => {
                        let addr = addr.expect("JmpAbs needs an address");
                        vm.goto(addr);
                        continue;
                    }
                    (_, Val::Bool(_)) => {
                        // Do nothing
                    }
                    _ => {
                        assert!(false, "Invalid types for JmpCond");
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
                        assert!(false, "Invalid types for Cast");
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
                        assert!(false, "Invalid types for Cast");
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
                        assert!(false, "Invalid types for Compare");
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
                        assert!(false, "Invalid types for Memory");
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
                            assert!(false, "Invalid types for MemoryWrite");
                        }
                    } else {
                        assert!(false, "Invalid types for MemoryWrite");
                    }
                } else {
                    assert!(false, "Invalid types for MemoryWrite");
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
                        assert!(false, "Invalid types for MemoryWrite");
                    }
                } else {
                    assert!(false, "Invalid types for MemoryWrite");
                }
            }
            Ops::Print => {
                let v = stack.pop().expect("Stack underflow - Print needs 1 value");
                match v {
                    Val::Int(i) => println!("{}", i),
                    Val::Bool(b) => println!("{}", b),
                    Val::Char(c) => println!("{}", c),
                    _ => {
                        assert!(false, "Invalid types for Print");
                    }
                }
            }
            Ops::Nop => {}
            Ops::Hlt => {
                break;
            }
        }

        vm.ip += 1;
    }

    println!("------------");
    println!("ip: {}", vm.ip);
    println!("stack: {:?}", stack);
}
