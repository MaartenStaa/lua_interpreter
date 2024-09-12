use std::ops;

use crate::{
    ast,
    value::{LuaNumber, LuaValue},
};

macro_rules! impl_number_ops {
    ($opname:ident, $op:tt, $method:ident, $wrapping_method:ident, $($target:tt)+) => {
        impl ops::$opname for $($target)+ {
            type Output = $($target)+;

            fn $method(self, other: Self) -> Self {
                match (self, other) {
                    ($($target)+::Integer(a), $($target)+::Integer(b)) => $($target)+::Integer(a.$wrapping_method(b)),
                    ($($target)+::Float(a), $($target)+::Float(b)) => $($target)+::Float(a $op b),
                    ($($target)+::Integer(a), $($target)+::Float(b)) => $($target)+::Float(a as f64 $op b),
                    ($($target)+::Float(a), $($target)+::Integer(b)) => $($target)+::Float(a $op b as f64),
                }
            }
        }

        impl ops::$opname for &$($target)+ {
            type Output = $($target)+;

            fn $method(self, other: Self) -> $($target)+ {
                match (self, other) {
                    ($($target)+::Integer(a), $($target)+::Integer(b)) => $($target)+::Integer(a.$wrapping_method(*b)),
                    ($($target)+::Float(a), $($target)+::Float(b)) => $($target)+::Float(a $op b),
                    ($($target)+::Integer(a), $($target)+::Float(b)) => $($target)+::Float(*a as f64 $op b),
                    ($($target)+::Float(a), $($target)+::Integer(b)) => $($target)+::Float(a $op *b as f64),
                }
            }
        }
    }
}

impl_number_ops!(Add, +, add, wrapping_add, ast::Number);
impl_number_ops!(Sub, -, sub, wrapping_sub, ast::Number);
impl_number_ops!(Mul, *, mul, wrapping_mul, ast::Number);
impl_number_ops!(Div, /, div, wrapping_div, ast::Number);
impl_number_ops!(Rem, %, rem, wrapping_rem, ast::Number);

impl ops::Neg for ast::Number {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            ast::Number::Integer(a) => ast::Number::Integer(-a),
            ast::Number::Float(a) => ast::Number::Float(-a),
        }
    }
}

impl ops::Neg for &ast::Number {
    type Output = ast::Number;

    fn neg(self) -> ast::Number {
        match self {
            ast::Number::Integer(a) => ast::Number::Integer(-*a),
            ast::Number::Float(a) => ast::Number::Float(-*a),
        }
    }
}

impl ast::Number {
    pub fn pow(&self, other: &Self) -> Self {
        match (self, other) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                ast::Number::Integer(a.wrapping_pow(*b as u32))
            }
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a.powf(*b)),
            (ast::Number::Integer(a), ast::Number::Float(b)) => {
                ast::Number::Float((*a as f64).powf(*b))
            }
            (ast::Number::Float(a), ast::Number::Integer(b)) => {
                ast::Number::Float(a.powi(*b as i32))
            }
        }
    }

    pub fn floor(self) -> Self {
        match self {
            ast::Number::Integer(a) => ast::Number::Integer(a),
            ast::Number::Float(a) => ast::Number::Integer(a.floor() as i64),
        }
    }

    pub fn to_string(self) -> String {
        match self {
            ast::Number::Integer(i) => i.to_string(),
            ast::Number::Float(f) => f.to_string(),
        }
    }
}

impl PartialOrd for ast::Number {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => a.partial_cmp(b),
            (ast::Number::Float(a), ast::Number::Float(b)) => a.partial_cmp(b),
            (ast::Number::Integer(a), ast::Number::Float(b)) => (*a as f64).partial_cmp(b),
            (ast::Number::Float(a), ast::Number::Integer(b)) => a.partial_cmp(&(*b as f64)),
        }
    }
}

impl ops::Add for LuaValue {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a + b),
            _ => unimplemented!(),
        }
    }
}

impl ops::Sub for LuaValue {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a - b),
            _ => unimplemented!(),
        }
    }
}

impl ops::Mul for LuaValue {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a * b),
            _ => unimplemented!(),
        }
    }
}

impl ops::Div for LuaValue {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a / b),
            _ => unimplemented!(),
        }
    }
}

impl ops::Rem for LuaValue {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a % b),
            _ => unimplemented!(),
        }
    }
}

impl ops::Neg for LuaValue {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            LuaValue::Number(a) => LuaValue::Number(-a),
            _ => unimplemented!(),
        }
    }
}

impl ops::Not for LuaValue {
    type Output = Self;

    fn not(self) -> Self {
        match self {
            LuaValue::Boolean(b) => LuaValue::Boolean(!b),
            LuaValue::Nil => LuaValue::Boolean(true),
            _ => LuaValue::Boolean(false),
        }
    }
}

impl ops::BitAnd for LuaValue {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a & b),
            _ => unimplemented!(),
        }
    }
}

impl ops::BitOr for LuaValue {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a | b),
            _ => unimplemented!(),
        }
    }
}

impl ops::BitXor for LuaValue {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a ^ b),
            _ => unimplemented!(),
        }
    }
}

impl ops::Shl for LuaValue {
    type Output = Self;

    fn shl(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a << b),
            _ => unimplemented!(),
        }
    }
}

impl ops::Shr for LuaValue {
    type Output = Self;

    fn shr(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a >> b),
            _ => unimplemented!(),
        }
    }
}

impl PartialOrd for LuaValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => a.partial_cmp(b),
            _ => todo!("LuaValue::partial_cmp"),
        }
    }
}

impl LuaValue {
    pub fn concat(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::String(mut a), LuaValue::String(b)) => {
                a.extend(b);
                LuaValue::String(a)
            }
            (LuaValue::String(mut a), LuaValue::Number(b)) => {
                a.extend(b.to_string().as_bytes());
                LuaValue::String(a)
            }
            (LuaValue::Number(a), LuaValue::String(b)) => {
                // b.insert_str(0, &a.to_string());
                LuaValue::String(Vec::from_iter(
                    a.to_string().into_bytes().into_iter().chain(b),
                ))
            }
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::String(Vec::from_iter(
                a.to_string()
                    .into_bytes()
                    .into_iter()
                    .chain(b.to_string().into_bytes()),
            )),
            _ => unimplemented!(),
        }
    }

    pub fn pow(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number(a.pow(b)),
            _ => unimplemented!(),
        }
    }

    pub fn idiv(self, other: Self) -> Self {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => LuaValue::Number((a / b).floor()),
            _ => unimplemented!(),
        }
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            LuaValue::Nil => false,
            LuaValue::Boolean(b) => *b,
            _ => true,
        }
    }
}

impl ops::Add for LuaNumber {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a.wrapping_add(b)),
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a + b),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float(a as f64 + b),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a + b as f64),
        }
    }
}

impl ops::Sub for LuaNumber {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a.wrapping_sub(b)),
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a - b),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float(a as f64 - b),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a - b as f64),
        }
    }
}

impl ops::Mul for LuaNumber {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a.wrapping_mul(b)),
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a * b),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float(a as f64 * b),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a * b as f64),
        }
    }
}

impl ops::Div for LuaNumber {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a.wrapping_div(b)),
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a / b),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float(a as f64 / b),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a / b as f64),
        }
    }
}

impl ops::Rem for LuaNumber {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a.wrapping_rem(b)),
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a % b),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float(a as f64 % b),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a % b as f64),
        }
    }
}

impl ops::Neg for LuaNumber {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            LuaNumber::Integer(a) => LuaNumber::Integer(-a),
            LuaNumber::Float(a) => LuaNumber::Float(-a),
        }
    }
}

impl ops::BitAnd for LuaNumber {
    type Output = Self;

    fn bitand(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a & b),
            // TODO: This is wrong. Need to check if float has a decimal part, if so it's an error
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Integer(a as i64 & b as i64),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Integer(a & b as i64),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a as i64 & b),
        }
    }
}

impl ops::BitOr for LuaNumber {
    type Output = Self;

    fn bitor(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a | b),
            _ => todo!("LuaNumber::bitor"),
        }
    }
}

impl ops::BitXor for LuaNumber {
    type Output = Self;

    fn bitxor(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a ^ b),
            _ => todo!("LuaNumber::bitxor"),
        }
    }
}

impl ops::Shl for LuaNumber {
    type Output = Self;

    fn shl(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a << b),
            _ => todo!("LuaNumber::shl"),
        }
    }
}

impl ops::Shr for LuaNumber {
    type Output = Self;

    fn shr(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Integer(a >> b),
            _ => todo!("LuaNumber::shr"),
        }
    }
}

impl PartialOrd for LuaNumber {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => a.partial_cmp(b),
            (LuaNumber::Float(a), LuaNumber::Float(b)) => a.partial_cmp(b),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => (*a as f64).partial_cmp(b),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => a.partial_cmp(&(*b as f64)),
        }
    }
}

impl LuaNumber {
    pub fn pow(self, other: Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => {
                LuaNumber::Integer(a.wrapping_pow(b as u32))
            }
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a.powf(b)),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float((a as f64).powf(b)),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a.powi(b as i32)),
        }
    }

    pub fn floor(self) -> Self {
        match self {
            LuaNumber::Integer(a) => LuaNumber::Integer(a),
            LuaNumber::Float(a) => LuaNumber::Float(a.floor()),
        }
    }
}
