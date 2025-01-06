#[derive(Clone, Debug, PartialEq)]
pub enum Type {
    Void,
    Char,
    Int,
    Ptr(Box<Type>),
    Array(Box<Type>, usize),
    Func(Box<Type>, Vec<Type>),
}

impl Type {
    pub fn sizeof(&self) -> usize {
        match self {
            Type::Void => 0,
            Type::Char => 1,
            Type::Int => 8,
            Type::Ptr(_) => 8,
            Type::Array(t, size) => t.sizeof() * size,
            Type::Func(_, _) => panic!("sizeof function is not allowed"),
        }
    }

    pub fn is_compatible_type(&self, other: &Self) -> bool {
        match (self, other) {
            (t1, t2) if t1 == t2 => true,
            (Type::Int, Type::Char) => true,
            (Type::Char, Type::Int) => true,
            _ => false,
        }
    }
}
