use std::ops;

use crate::{
    ast,
    value::{LuaNumber, LuaValue},
};

impl ops::Add for ast::Number {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        match (self, other) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                ast::Number::Integer(a.wrapping_add(b))
            }
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a + b),
            (ast::Number::Integer(a), ast::Number::Float(b)) => ast::Number::Float(a as f64 + b),
            (ast::Number::Float(a), ast::Number::Integer(b)) => ast::Number::Float(a + b as f64),
        }
    }
}

impl ops::Sub for ast::Number {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        match (self, other) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                ast::Number::Integer(a.wrapping_sub(b))
            }
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a - b),
            (ast::Number::Integer(a), ast::Number::Float(b)) => ast::Number::Float(a as f64 - b),
            (ast::Number::Float(a), ast::Number::Integer(b)) => ast::Number::Float(a - b as f64),
        }
    }
}

impl ops::Mul for ast::Number {
    type Output = Self;

    fn mul(self, other: Self) -> Self {
        match (self, other) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                ast::Number::Integer(a.wrapping_mul(b))
            }
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a * b),
            (ast::Number::Integer(a), ast::Number::Float(b)) => ast::Number::Float(a as f64 * b),
            (ast::Number::Float(a), ast::Number::Integer(b)) => ast::Number::Float(a * b as f64),
        }
    }
}

impl ops::Div for ast::Number {
    type Output = Self;

    fn div(self, other: Self) -> Self {
        match (self, other) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                ast::Number::Integer(a.wrapping_div(b))
            }
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a / b),
            (ast::Number::Integer(a), ast::Number::Float(b)) => ast::Number::Float(a as f64 / b),
            (ast::Number::Float(a), ast::Number::Integer(b)) => ast::Number::Float(a / b as f64),
        }
    }
}

impl ops::Rem for ast::Number {
    type Output = Self;

    fn rem(self, other: Self) -> Self {
        match (self, other) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                ast::Number::Integer(a.wrapping_rem(b))
            }
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a % b),
            (ast::Number::Integer(a), ast::Number::Float(b)) => ast::Number::Float(a as f64 % b),
            (ast::Number::Float(a), ast::Number::Integer(b)) => ast::Number::Float(a % b as f64),
        }
    }
}

impl ops::Neg for ast::Number {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            ast::Number::Integer(a) => ast::Number::Integer(-a),
            ast::Number::Float(a) => ast::Number::Float(-a),
        }
    }
}

impl ast::Number {
    pub fn pow(self, other: Self) -> Self {
        match (self, other) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                ast::Number::Integer(a.wrapping_pow(b as u32))
            }
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a.powf(b)),
            (ast::Number::Integer(a), ast::Number::Float(b)) => {
                ast::Number::Float((a as f64).powf(b))
            }
            (ast::Number::Float(a), ast::Number::Integer(b)) => {
                ast::Number::Float(a.powi(b as i32))
            }
        }
    }

    pub fn floor(self) -> Self {
        match self {
            ast::Number::Integer(a) => ast::Number::Integer(a),
            ast::Number::Float(a) => ast::Number::Integer(a.floor() as i64),
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
