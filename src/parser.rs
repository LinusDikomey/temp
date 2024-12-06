use std::rc::Rc;

use lexer::Lexer;

use crate::{error::Errors, Expr, Op};

mod lexer;

pub use lexer::TokenType;

pub struct Ast {
    pub top_level: Vec<Expr>,
}

pub fn parse(source: &str, errors: &mut Errors) -> Ast {
    let lexer = Lexer {
        chars: source.chars().peekable(),
        peeked: None,
    };
    let mut ast = Ast {
        top_level: Vec::new(),
    };
    let mut parser = Parser {
        ast: &mut ast,
        lexer,
        errors,
    };

    let items = parser.parse_file();
    ast.top_level = items;
    ast
}

struct Parser<'a> {
    lexer: Lexer<'a>,
    ast: &'a mut Ast,
    errors: &'a mut Errors,
}

impl<'a> Parser<'a> {
    fn parse_file(&mut self) -> Vec<Expr> {
        let mut exprs = Vec::new();
        loop {
            if self.lexer.peek().ty == TokenType::Eof {
                break;
            }
            exprs.push(self.parse_expr());
        }
        exprs
    }

    fn parse_expr(&mut self) -> Expr {
        let lhs = self.parse_factor();
        self.parse_bin_op_rhs(0, lhs)
    }

    fn parse_factor(&mut self) -> Expr {
        let tok = self.lexer.next();
        let mut expr = match tok.ty {
            TokenType::Ident(name) => {
                if self.lexer.peek().ty == TokenType::Equals {
                    self.lexer.next();
                    let value = self.parse_expr();
                    Expr::Assign(name, Box::new(value))
                } else {
                    Expr::Ident(name)
                }
            }
            TokenType::Number(n) => Expr::Number(n),
            tok => panic!("unexpected token while parsing expr: {tok:?}"),
        };
        loop {
            expr = match self.lexer.peek().ty {
                TokenType::LParen => {
                    let args = self.parse_arguments();
                    Expr::Call(Box::new(expr), args)
                }
                TokenType::Arrow => {
                    self.lexer.next();
                    let params = match expr {
                        Expr::Ident(name) => vec![name],
                        _ => panic!("invalid param expression for function definition"),
                    };
                    let body = self.parse_expr();
                    Expr::Function(Rc::new((params, body)))
                }
                TokenType::Dot => {
                    self.lexer.next();
                    let name = self.lexer.next();
                    let TokenType::Ident(name) = name.ty else {
                        panic!("name expected after dot");
                    };
                    let args = if self.lexer.peek().ty == TokenType::LParen {
                        self.parse_arguments()
                    } else {
                        vec![self.parse_expr()]
                    };
                    Expr::Method(Box::new(expr), name, args)
                }
                _ => break expr,
            };
        }
    }

    fn parse_arguments(&mut self) -> Vec<Expr> {
        let lparen = self.lexer.next();
        if lparen.ty != TokenType::LParen {
            panic!("expected lparen");
        }
        let mut args = Vec::new();
        loop {
            if self.lexer.peek().ty == TokenType::RParen {
                self.lexer.next();
                break;
            }
            args.push(self.parse_expr());
        }
        args
    }

    fn parse_bin_op_rhs(&mut self, expr_prec: u32, mut lhs: Expr) -> Expr {
        while let Ok(op) = Op::try_from(&self.lexer.peek().ty) {
            self.lexer.next();
            let op_prec = op.precedence();
            if op_prec < expr_prec {
                break;
            }
            let mut rhs = self.parse_factor();
            if let Ok(next_op) = Op::try_from(&self.lexer.peek().ty) {
                if op_prec < next_op.precedence() {
                    rhs = self.parse_bin_op_rhs(op.precedence() + 1, rhs);
                }
            }
            lhs = Expr::BinOp(Box::new(lhs), op, Box::new(rhs));
        }
        lhs
    }
}
