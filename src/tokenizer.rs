#[derive(Clone, Copy)]
pub struct Loc {
    pub line: usize,
    pub col: usize,
}

#[allow(unused_macros)]
macro_rules! lprintln {
    ($filename:expr,$loc:expr, $($arg:tt)*) => {{
        let filename = $filename;
        let loc: Loc = $loc;

        println!("{}:{}:{}: {}", filename, loc.line, loc.col, format_args!($($arg)*));
    }};
}

#[allow(unused_macros)]
macro_rules! leprintln {
    ($filename:expr,$loc:expr, $($arg:tt)*) => {{
        let filename = $filename;
        let loc: Loc = $loc;

        eprintln!("{}:{}:{}: ERROR: {}", filename, loc.line, loc.col, format_args!($($arg)*));
    }};
}

pub struct Token<'a> {
    pub loc: Loc,
    pub value: &'a str,
}

pub struct Tokenizer<'a> {
    pub input: &'a str,
    pub line: usize,
    pub col: usize,
    pub pos: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn new(input: &'a str) -> Tokenizer<'a> {
        Tokenizer {
            input,
            line: 1,
            col: 1,
            pos: 0,
        }
    }

    pub fn to_vec<'n>(&'n mut self) -> Vec<Token<'n>> {
        self.collect::<Vec<_>>()
    }

    pub fn next_token(&mut self) -> Option<Token<'a>> {
        let mut start = None;
        let mut col = 1;

        for c in self.input[self.pos..].chars() {
            if !c.is_whitespace() && start.is_none() {
                start = Some(self.pos);
                col = self.col;
            }
            if c.is_whitespace() && start.is_some() {
                break;
            }
            if c == '\n' {
                self.line += 1;
                self.col = 0;
            }
            self.col += 1;
            self.pos += c.len_utf8();
        }

        if let Some(start_pos) = start {
            if start_pos != self.pos {
                return Some(Token {
                    loc: Loc {
                        line: self.line,
                        col,
                    },
                    value: &self.input[start_pos..self.pos],
                });
            }
        }

        None
    }
}

impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;
    fn next(&mut self) -> Option<Self::Item> {
        self.next_token()
    }
}
