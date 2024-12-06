use std::{collections::HashMap, fmt, rc::Rc};

use crate::{Expr, Op};

pub fn interp(ast: &crate::parser::Ast) {
    let builtins = [
        ("list", Builtin::List),
        ("map", Builtin::Map),
        ("filter", Builtin::Filter),
        ("print", Builtin::Print),
        ("if", Builtin::If),
        ("else", Builtin::Else),
        ("read", Builtin::Read),
    ];
    let mut vars: HashMap<_, _> = builtins
        .into_iter()
        .map(|(name, builtin)| (name.to_owned(), Value::Function(Function::Builtin(builtin))))
        .collect();
    vars.insert("none".to_owned(), Value::None);
    let mut interp = Interp { vars };
    for expr in &ast.top_level {
        interp.eval(expr);
    }
}

struct Interp {
    vars: HashMap<String, Value>,
}
impl Interp {
    fn eval_lazy(&mut self, lazy: Lazy) -> Value {
        match lazy {
            Lazy::Expr(expr) => self.eval(expr),
            Lazy::Value(value) => value,
        }
    }

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
            Expr::String(s) => Value::String(s.clone()),
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
                let func = self.eval(called);
                self.call_function(func, args.iter().map(Lazy::Expr))
            }
            Expr::Method(lhs, name, args) => {
                let func = self
                    .vars
                    .get(name)
                    .cloned()
                    .unwrap_or_else(|| panic!("unknown method name '{name}'"));
                self.call_function(
                    func,
                    std::iter::once(&**lhs).chain(args.iter()).map(Lazy::Expr),
                )
            }
            Expr::Function(func) => Value::Function(Function::UserDefined(func.clone())),
            Expr::Tuple(args) => Value::List(args.iter().map(|arg| self.eval(arg)).collect()),
        }
    }

    fn call_function<'a>(
        &mut self,
        function: Value,
        args: impl IntoIterator<Item = Lazy<'a>>,
    ) -> Value {
        match function {
            Value::Function(Function::Builtin(builtin)) => builtin.call(self, args),
            Value::Function(Function::UserDefined(func)) => {
                let mut args = args.into_iter();
                let mut count = 0;
                for (name, value) in func.0.iter().zip(&mut args) {
                    count += 1;
                    let value = self.eval_lazy(value);
                    self.vars.insert(name.clone(), value);
                }
                if count < func.0.len() {
                    panic!(
                        "not enough arguments, expected {} but got {}",
                        func.0.len(),
                        count
                    );
                }
                let extra = args.count();
                if extra != 0 {
                    panic!(
                        "too many arguments, expected {count} but got {}",
                        count + extra
                    );
                }
                self.eval(&func.1)
            }
            Value::List(list) => {
                let mut args = args.into_iter();
                let (Some(index), None) = (args.next(), args.next()) else {
                    panic!("expected one argument for list index");
                };
                let Value::Number(n) = self.eval_lazy(index) else {
                    panic!("expected an integer index");
                };
                list.get(n as usize)
                    .expect("list index out of bounds")
                    .clone()
            }
            Value::String(s) => {
                let mut args = args.into_iter();
                let (Some(index), None) = (args.next(), args.next()) else {
                    panic!("expected one argument for list index");
                };
                let Value::Number(n) = self.eval_lazy(index) else {
                    panic!("expected an integer index");
                };
                let &byte = s
                    .as_bytes()
                    .get(n as usize)
                    .expect("string index out of bounds");
                Value::String(
                    String::from_utf8(vec![byte])
                        .expect("invalid codepoint at index")
                        .into(),
                )
            }
            value => panic!("can't call value {value}"),
        }
    }
}

enum Lazy<'a> {
    Expr(&'a Expr),
    Value(Value),
}

#[derive(Clone, Debug)]
pub enum Value {
    Number(i64),
    String(Rc<str>),
    Function(Function),
    List(Vec<Value>),
    None,
    IfNone,
}
impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::Number(n) => write!(f, "{n}"),
            Value::String(s) => write!(f, "{s}"),
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
            Value::None => write!(f, "none"),
            Value::IfNone => write!(f, "ifnone"),
        }
    }
}
impl Value {
    fn truthy(&self) -> bool {
        match self {
            &Value::Number(n) => n != 0,
            Value::String(s) => !s.is_empty(),
            Value::Function(_) => true,
            Value::List(l) => !l.is_empty(),
            Value::None => false,
            Value::IfNone => false,
        }
    }
}
impl PartialEq for Value {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(x), Self::Number(y)) => x == y,
            (Self::String(x), Self::String(y)) => x == y,
            (Self::Function(_), Self::Function(_)) => {
                panic!("don't compare functions you idiot")
            }
            (Self::List(x), Self::List(y)) => {
                x.len() == y.len() && x.iter().zip(y).all(|(a, b)| a == b)
            }
            (Self::None, Self::None) => true,
            (Self::IfNone, Self::IfNone) => true,
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
    If,
    Else,
    Read,
}
impl Builtin {
    fn call<'a>(&self, interp: &mut Interp, args: impl IntoIterator<Item = Lazy<'a>>) -> Value {
        let mut args = args.into_iter();
        match self {
            Builtin::List => Value::List(args.map(|arg| interp.eval_lazy(arg)).collect()),
            Builtin::Map => {
                let (Some(a), Some(b), None) = (args.next(), args.next(), args.next()) else {
                    panic!("invalid argument count for map, expected 2");
                };
                let [Value::List(list), func] = [a, b].map(|v| interp.eval_lazy(v)) else {
                    panic!("expected 2 arguments of type (list, function) map");
                };
                let list = list
                    .iter()
                    .map(|item| interp.call_function(func.clone(), [Lazy::Value(item.clone())]))
                    .collect();
                Value::List(list)
            }
            Builtin::Filter => {
                let (Some(a), Some(b), None) = (args.next(), args.next(), args.next()) else {
                    panic!("invalid argument count for map, expected 2");
                };
                let [Value::List(list), func] = [a, b].map(|v| interp.eval_lazy(v)) else {
                    panic!("expected 2 arguments of type (list, function) filter");
                };
                let list = list
                    .iter()
                    .filter(|&item| {
                        interp
                            .call_function(func.clone(), [Lazy::Value(item.clone())])
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
                    let value = interp.eval_lazy(arg);
                    print!("{value}");
                }
                println!();
                Value::None
            }
            Builtin::If => {
                let cond = args.next().expect("if needs a condition");
                if interp.eval_lazy(cond).truthy() {
                    let mut value = Value::None;
                    for arg in args {
                        value = interp.eval_lazy(arg);
                    }
                    if value == Value::IfNone {
                        Value::None
                    } else {
                        value
                    }
                } else {
                    Value::IfNone
                }
            }
            Builtin::Else => {
                let cond = args.next().expect("else needs a condition");
                let cond = interp.eval_lazy(cond);
                if cond == Value::IfNone {
                    let mut value = Value::None;
                    for arg in args {
                        value = interp.eval_lazy(arg);
                    }
                    value
                } else {
                    cond
                }
            }
            Builtin::Read => {
                let (Some(file), None) = (args.next(), args.next()) else {
                    panic!("expected 1 argument for read");
                };
                let Value::String(file) = interp.eval_lazy(file) else {
                    panic!("expected a string for read");
                };
                std::fs::read_to_string(&*file).map_or(Value::None, |s| Value::String(s.into()))
            }
        }
    }
}
