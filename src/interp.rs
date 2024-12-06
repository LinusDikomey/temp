use std::{collections::HashMap, fmt, rc::Rc};

use crate::{Expr, Op};

pub fn interp(ast: &crate::parser::Ast) {
    let builtins = [
        ("list", Builtin::List),
        ("map", Builtin::Map),
        ("filter", Builtin::Filter),
        ("print", Builtin::Print),
    ];
    let vars = builtins
        .into_iter()
        .map(|(name, builtin)| (name.to_owned(), Value::Function(Function::Builtin(builtin))))
        .collect();
    let mut interp = Interp { vars };
    for expr in &ast.top_level {
        interp.eval(expr);
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
                .unwrap_or_else(|| panic!("undefined variable '{name}'"))
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
                let n = match op {
                    Op::Equals => (l == r) as i64,
                    Op::NotEquals => (l != r) as i64,
                    _ => {
                        let (Value::Number(l), Value::Number(r)) = (l, r) else {
                            panic!("invalid types for operator {op:?}");
                        };
                        match op {
                            Op::Equals | Op::NotEquals => unreachable!(),
                            Op::Add => l + r,
                            Op::Sub => l - r,
                            Op::Mul => l * r,
                            Op::Div => l / r,
                            Op::Mod => l % r,
                            Op::LT => (l < r) as i64,
                            Op::LE => (l <= r) as i64,
                            Op::GT => (l > r) as i64,
                            Op::GE => (l >= r) as i64,
                        }
                    }
                };
                Value::Number(n)
            }
            Expr::Call(called, args) => {
                let Value::Function(func) = self.eval(called) else {
                    panic!("can't call non-function");
                };
                let args: Vec<Value> = args.iter().map(|arg| self.eval(arg)).collect();
                self.call_function(&func, args.into_iter())
            }
            Expr::Method(lhs, name, args) => {
                let Value::Function(func) = self
                    .vars
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| panic!("unknown method name '{name}'"))
                else {
                    panic!("can't call non-function");
                };
                let args: Vec<Value> = std::iter::once(&**lhs)
                    .chain(args.iter())
                    .map(|arg| self.eval(arg))
                    .collect();
                self.call_function(&func, args.into_iter())
            }
            Expr::Function(func) => Value::Function(Function::UserDefined(func.clone())),
        }
    }

    fn call_function(
        &mut self,
        function: &Function,
        args: impl ExactSizeIterator<Item = Value>,
    ) -> Value {
        match function {
            Function::Builtin(builtin) => builtin.call(self, args.collect()),
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
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(i64),
    Function(Function),
    List(Vec<Value>),
    None,
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::Function(_) => write!(f, "<function>"),
            Value::List(list) => {
                write!(f, "[")?;
                let mut first = true;
                for item in list {
                    if first {
                        first = false;
                    } else {
                        write!(f, " ")?;
                    }
                    write!(f, "{item}")?;
                }
                write!(f, "]")
            }
            Value::None => todo!(),
        }
    }
}
impl Value {
    fn truthy(&self) -> bool {
        match self {
            &Value::Number(n) => n != 0,
            Value::Function(_) => true,
            Value::List(l) => !l.is_empty(),
            Value::None => false,
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(x), Self::Number(y)) => x == y,
            (Self::Function(_), Self::Function(_)) => {
                panic!("don't compare functions you idiot")
            }
            (Self::List(x), Self::List(y)) => {
                x.len() == y.len() && x.iter().zip(y).all(|(a, b)| a == b)
            }
            _ => false,
        }
    }
}

#[derive(Clone, Debug)]
pub enum Function {
    Builtin(Builtin),
    UserDefined(Rc<(Vec<String>, Expr)>),
}

#[derive(Clone, Copy, Debug)]
pub enum Builtin {
    List,
    Map,
    Filter,
    Print,
}
impl Builtin {
    fn call(&self, interp: &mut Interp, args: Vec<Value>) -> Value {
        match self {
            Builtin::List => Value::List(args),
            Builtin::Map => {
                let [Value::List(list), Value::Function(func)] = args.as_slice() else {
                    panic!("expected 2 arguments of type (list, function) map");
                };
                let list = list
                    .iter()
                    .map(|item| interp.call_function(func, [item.clone()].into_iter()))
                    .collect();
                Value::List(list)
            }
            Builtin::Filter => {
                let [Value::List(list), Value::Function(func)] = args.as_slice() else {
                    panic!("expected 2 arguments of type (list, function) filter");
                };
                let list = list
                    .iter()
                    .filter(|&item| {
                        interp
                            .call_function(func, [item.clone()].into_iter())
                            .truthy()
                    })
                    .cloned()
                    .collect();
                Value::List(list)
            }
            Builtin::Print => {
                let mut first = true;
                for arg in args {
                    if first {
                        first = false;
                    } else {
                        print!(" ");
                    }
                    print!("{arg}");
                }
                Value::None
            }
        }
    }
}
