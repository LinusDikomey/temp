use std::{collections::HashMap, rc::Rc};

use crate::Expr;

pub fn interp(ast: &crate::parser::Ast) {
    let mut interp = Interp {
        vars: HashMap::new(),
    };
    interp.vars.insert(
        "list".to_owned(),
        Value::Function(Function::Builtin(Builtin::List)),
    );
    for expr in &ast.top_level {
        let value = interp.eval(expr);
        let v = value;
        if !matches!(v, Value::None) {
            println!("{v:?}");
        }
    }
}

struct Interp {
    vars: HashMap<String, Value>,
}
impl Interp {
    fn eval(&mut self, expr: &Expr) -> Value {
        match expr {
            Expr::Ident(name) => self
                .vars
                .get(name)
                .unwrap_or_else(|| panic!("undefined variable {name}"))
                .clone(),
            Expr::Assign(name, value) => {
                let value = self.eval(value);
                self.vars.insert(name.to_owned(), value.clone());
                value
            }
            &Expr::Number(n) => Value::Number(n.try_into().unwrap()),
            Expr::BinOp(l, op, r) => {
                let l = self.eval(l);
                let r = self.eval(r);
                let (Value::Number(l), Value::Number(r)) = (l, r) else {
                    panic!("invalid types for operator {op:?}");
                };
                let n = match op {
                    crate::Op::Add => l + r,
                    crate::Op::Sub => l - r,
                    crate::Op::Mul => l * r,
                    crate::Op::Div => l / r,
                    crate::Op::Mod => l % r,
                };
                Value::Number(n)
            }
            Expr::Call(called, args) => {
                let Value::Function(func) = self.eval(called) else {
                    panic!("can't call non-function");
                };
                let args: Vec<Value> = args.iter().map(|arg| self.eval(arg)).collect();
                match func {
                    Function::Builtin(builtin) => builtin.call(args),
                    Function::UserDefined(func) => {
                        if args.len() != func.0.len() {
                            panic!("invalid argument count");
                        }
                        for (name, value) in func.0.iter().zip(args) {
                            self.vars.insert(name.clone(), value);
                        }
                        self.eval(&func.1)
                    }
                }
            }
            Expr::Function(func) => Value::Function(Function::UserDefined(func.clone())),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(i64),
    Function(Function),
    List(Vec<Value>),
    None,
}

#[derive(Clone, Debug)]
pub enum Function {
    Builtin(Builtin),
    UserDefined(Rc<(Vec<String>, Expr)>),
}

#[derive(Clone, Copy, Debug)]
pub enum Builtin {
    List,
}
impl Builtin {
    fn call(&self, args: Vec<Value>) -> Value {
        match self {
            Builtin::List => Value::List(args),
        }
    }
}
