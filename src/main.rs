use std::rc::Rc;

use error::Errors;
use parser::TokenType;

mod error;
mod interp;
mod parser;

fn main() {
    let arg = std::env::args()
        .skip(1)
        .next()
        .expect("file argument expected");
    let file = std::fs::read_to_string(arg).unwrap();
    let mut errors = Errors { errors: Vec::new() };
    let ast = parser::parse(&file, &mut errors);
    interp::interp(&ast);
}

#[derive(Debug)]
pub enum Expr {
    Ident(String),
    Assign(String, Box<Expr>),
    Number(u64),
    BinOp(Box<Expr>, Op, Box<Expr>),
    Call(Box<Expr>, Vec<Expr>),
    Function(Rc<(Vec<String>, Expr)>),
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
}
impl Op {
    fn precedence(&self) -> u32 {
        match self {
            Op::Add | Op::Sub => 70,
            Op::Mul | Op::Div | Op::Mod => 80,
        }
    }
}
impl TryFrom<&TokenType> for Op {
    type Error = ();
    fn try_from(value: &TokenType) -> Result<Self, Self::Error> {
        Ok(match value {
            TokenType::Plus => Op::Add,
            TokenType::Minus => Op::Sub,
            TokenType::Star => Op::Mul,
            TokenType::Slash => Op::Div,
            TokenType::Percent => Op::Mod,
            _ => return Err(()),
        })
    }
}
