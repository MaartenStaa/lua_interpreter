use std::{
    fmt::{self, Display},
    ops,
};

use crate::{
    ast,
    error::{lua_error, Context},
    value::{LuaNumber, LuaObject, LuaValue},
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

impl ops::Rem for ast::Number {
    type Output = ast::Number;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                if a < 0 {
                    ast::Number::Integer(b - a.abs().wrapping_rem(b))
                } else {
                    ast::Number::Integer(a.wrapping_rem(b))
                }
            }
            // TODO: Are these right?
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a % b),
            (ast::Number::Integer(a), ast::Number::Float(b)) => ast::Number::Float(a as f64 % b),
            (ast::Number::Float(a), ast::Number::Integer(b)) => ast::Number::Float(a % b as f64),
        }
    }
}

impl ops::Rem for &ast::Number {
    type Output = ast::Number;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                if *a < 0 {
                    ast::Number::Integer(b - a.abs().wrapping_rem(*b))
                } else {
                    ast::Number::Integer(a.wrapping_rem(*b))
                }
            }
            // TODO: Are these right?
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a % b),
            (ast::Number::Integer(a), ast::Number::Float(b)) => ast::Number::Float(*a as f64 % b),
            (ast::Number::Float(a), ast::Number::Integer(b)) => ast::Number::Float(a % *b as f64),
        }
    }
}

impl ops::Div for ast::Number {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        if matches!(&rhs, ast::Number::Integer(0) | ast::Number::Float(0.0)) {
            return ast::Number::Float(if matches!(self, ast::Number::Integer(0)) {
                f64::NAN
            } else {
                f64::INFINITY
            });
        }

        match (self, rhs) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                ast::Number::Float(a as f64 / b as f64)
            }
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a / b),
            (ast::Number::Integer(a), ast::Number::Float(b)) => ast::Number::Float(a as f64 / b),
            (ast::Number::Float(a), ast::Number::Integer(b)) => ast::Number::Float(a / b as f64),
        }
    }
}

impl ops::Div for &ast::Number {
    type Output = ast::Number;

    fn div(self, rhs: Self) -> Self::Output {
        if matches!(&rhs, ast::Number::Integer(0) | ast::Number::Float(0.0)) {
            return ast::Number::Float(if matches!(self, ast::Number::Integer(0)) {
                f64::NAN
            } else {
                f64::INFINITY
            });
        }

        match (self, rhs) {
            (ast::Number::Integer(a), ast::Number::Integer(b)) => {
                ast::Number::Float(*a as f64 / *b as f64)
            }
            (ast::Number::Float(a), ast::Number::Float(b)) => ast::Number::Float(a / b),
            (ast::Number::Integer(a), ast::Number::Float(b)) => ast::Number::Float(*a as f64 / b),
            (ast::Number::Float(a), ast::Number::Integer(b)) => ast::Number::Float(a / *b as f64),
        }
    }
}

impl ops::Neg for ast::Number {
    type Output = Self;

    fn neg(self) -> Self {
        match self {
            ast::Number::Integer(a) => ast::Number::Integer(a.wrapping_neg()),
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

    pub fn is_zero(&self) -> bool {
        match self {
            ast::Number::Integer(i) => *i == 0,
            ast::Number::Float(f) => *f == 0.0,
        }
    }
}

impl Display for ast::Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            ast::Number::Integer(i) => write!(f, "{}", i),
            ast::Number::Float(n) => write!(f, "{}", n),
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

impl ops::Not for LuaValue {
    type Output = bool;

    fn not(self) -> bool {
        match self {
            LuaValue::Boolean(b) => !b,
            LuaValue::Nil => true,
            _ => false,
        }
    }
}

impl ops::Not for &LuaValue {
    type Output = bool;

    fn not(self) -> bool {
        match self {
            LuaValue::Boolean(b) => !b,
            LuaValue::Nil => true,
            _ => false,
        }
    }
}

impl ops::Shl for LuaValue {
    type Output = crate::Result<Self>;

    fn shl(self, other: Self) -> crate::Result<Self> {
        match (&self, &other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => Ok(LuaValue::Number((a << b)?)),
            _ => {
                let left_type = self.type_name();
                let right_type = other.type_name();

                Err(lua_error!(
                    "cannot bitwise shift left a '{}' with a '{}'",
                    left_type,
                    right_type
                ))
            }
        }
    }
}

impl ops::Shr for LuaValue {
    type Output = crate::Result<Self>;

    fn shr(self, other: Self) -> crate::Result<Self> {
        match (&self, &other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => Ok(LuaValue::Number((a >> b)?)),
            _ => {
                let left_type = self.type_name();
                let right_type = other.type_name();

                Err(lua_error!(
                    "cannot bitwise shift right a '{}' with a '{}'",
                    left_type,
                    right_type
                ))
            }
        }
    }
}

impl PartialOrd for LuaValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => a.partial_cmp(b),
            (LuaValue::String(a), LuaValue::String(b)) => a.partial_cmp(b),
            _ => todo!("LuaValue::partial_cmp (metatables)"),
        }
    }
}

impl LuaValue {
    pub fn concat(self, other: Self) -> crate::Result<Self> {
        match (self, other) {
            (LuaValue::String(mut a), LuaValue::String(b)) => {
                a.extend(b);
                Ok(LuaValue::String(a))
            }
            (LuaValue::String(mut a), LuaValue::Number(b)) => {
                a.extend(b.to_string().as_bytes());
                Ok(LuaValue::String(a))
            }
            (LuaValue::Number(a), LuaValue::String(b)) => {
                // b.insert_str(0, &a.to_string());
                Ok(LuaValue::String(Vec::from_iter(
                    a.to_string().into_bytes().into_iter().chain(b),
                )))
            }
            (LuaValue::Number(a), LuaValue::Number(b)) => Ok(LuaValue::String(Vec::from_iter(
                a.to_string()
                    .into_bytes()
                    .into_iter()
                    .chain(b.to_string().into_bytes()),
            ))),
            (self_, other) => {
                let left_type = self_.type_name();
                let right_type = other.type_name();

                Err(lua_error!(
                    "cannot concatenate a '{}' with a '{}'",
                    left_type,
                    right_type
                ))
            }
        }
    }

    pub fn pow(self, other: Self) -> crate::Result<Self> {
        match (&self, &other) {
            (LuaValue::Number(a), LuaValue::Number(b)) => Ok(LuaValue::Number(a.pow(b))),
            _ => {
                let left_type = self.type_name();
                let right_type = other.type_name();

                Err(lua_error!(
                    "cannot raise a '{}' to the power of a '{}'",
                    left_type,
                    right_type
                ))
            }
        }
    }

    pub fn as_boolean(&self) -> bool {
        match self {
            LuaValue::Nil => false,
            LuaValue::Boolean(b) => *b,
            _ => true,
        }
    }

    pub fn len(&self) -> crate::Result<LuaValue> {
        match self {
            LuaValue::String(s) => Ok(LuaValue::Number(LuaNumber::Integer(s.len() as i64))),
            LuaValue::Object(o) => {
                let len = o.read().unwrap().len()?;
                Ok(LuaValue::Number(LuaNumber::Integer(len as i64)))
            }
            _ => Err(lua_error!("attempt to get length of a non-string value")),
        }
    }

    pub fn bitwise_not(&self) -> crate::Result<Self> {
        match self {
            LuaValue::Number(n) => Ok(LuaValue::Number((!n)?)),
            _ => Err(lua_error!(
                "attempt to perform bitwise not on a non-number value"
            )),
        }
    }
}

impl LuaObject {
    pub fn is_empty(&self) -> bool {
        match self {
            LuaObject::Table(t) => t.is_empty(),
            _ => false,
        }
    }

    pub fn len(&self) -> crate::Result<usize> {
        match self {
            LuaObject::Table(t) => Ok(t.len()),
            _ => Err(lua_error!("attempt to get length of a non-string value")),
        }
    }
}

impl_number_ops!(Add, +, add, wrapping_add, LuaNumber);
impl_number_ops!(Sub, -, sub, wrapping_sub, LuaNumber);
impl_number_ops!(Mul, *, mul, wrapping_mul, LuaNumber);

impl ops::Rem for LuaNumber {
    type Output = LuaNumber;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => {
                if a < 0 {
                    LuaNumber::Integer(b - a.abs().wrapping_rem(b))
                } else {
                    LuaNumber::Integer(a.wrapping_rem(b))
                }
            }
            // TODO: Are these right?
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a % b),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float(a as f64 % b),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a % b as f64),
        }
    }
}

impl ops::Rem for &LuaNumber {
    type Output = LuaNumber;

    fn rem(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => {
                if *a < 0 {
                    LuaNumber::Integer(b - a.abs().wrapping_rem(*b))
                } else {
                    LuaNumber::Integer(a.wrapping_rem(*b))
                }
            }
            // TODO: Are these right?
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a % b),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float(*a as f64 % b),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a % *b as f64),
        }
    }
}

impl ops::Div for LuaNumber {
    type Output = Self;

    fn div(self, rhs: Self) -> Self::Output {
        if matches!(&rhs, LuaNumber::Integer(0) | LuaNumber::Float(0.0)) {
            return LuaNumber::Float(if matches!(self, LuaNumber::Integer(0)) {
                f64::NAN
            } else {
                f64::INFINITY
            });
        }

        match (self, rhs) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => LuaNumber::Float(a as f64 / b as f64),
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a / b),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float(a as f64 / b),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a / b as f64),
        }
    }
}

impl ops::Div for &LuaNumber {
    type Output = LuaNumber;

    fn div(self, rhs: Self) -> Self::Output {
        if matches!(&rhs, LuaNumber::Integer(0) | LuaNumber::Float(0.0)) {
            return LuaNumber::Float(if matches!(self, LuaNumber::Integer(0)) {
                f64::NAN
            } else {
                f64::INFINITY
            });
        }

        match (self, rhs) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => {
                LuaNumber::Float(*a as f64 / *b as f64)
            }
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a / b),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float(*a as f64 / b),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a / *b as f64),
        }
    }
}

impl ops::Neg for LuaNumber {
    type Output = LuaNumber;

    fn neg(self) -> LuaNumber {
        match self {
            LuaNumber::Integer(a) => LuaNumber::Integer(a.saturating_neg()),
            LuaNumber::Float(a) => LuaNumber::Float(-a),
        }
    }
}

impl ops::Neg for &LuaNumber {
    type Output = LuaNumber;

    fn neg(self) -> LuaNumber {
        match self {
            LuaNumber::Integer(a) => LuaNumber::Integer(-a),
            LuaNumber::Float(a) => LuaNumber::Float(-a),
        }
    }
}

impl ops::BitAnd for LuaNumber {
    type Output = crate::Result<LuaNumber>;

    fn bitand(self, other: Self) -> crate::Result<LuaNumber> {
        match (self.integer_repr(), other.integer_repr()) {
            (Ok(a), Ok(b)) => Ok(LuaNumber::Integer(a & b)),
            (Err(e), _) | (_, Err(e)) => Err(e).wrap_err("during bitwise and"),
        }
    }
}

impl ops::BitAnd for &LuaNumber {
    type Output = crate::Result<LuaNumber>;

    fn bitand(self, other: Self) -> crate::Result<LuaNumber> {
        match (self.integer_repr(), other.integer_repr()) {
            (Ok(a), Ok(b)) => Ok(LuaNumber::Integer(a & b)),
            (Err(e), _) | (_, Err(e)) => Err(e).wrap_err("during bitwise and"),
        }
    }
}

impl ops::BitOr for LuaNumber {
    type Output = crate::Result<LuaNumber>;

    fn bitor(self, other: Self) -> crate::Result<LuaNumber> {
        match (self.integer_repr(), other.integer_repr()) {
            (Ok(a), Ok(b)) => Ok(LuaNumber::Integer(a | b)),
            (Err(e), _) | (_, Err(e)) => Err(e).wrap_err("during bitwise or"),
        }
    }
}

impl ops::BitOr for &LuaNumber {
    type Output = crate::Result<LuaNumber>;

    fn bitor(self, other: Self) -> crate::Result<LuaNumber> {
        match (self.integer_repr(), other.integer_repr()) {
            (Ok(a), Ok(b)) => Ok(LuaNumber::Integer(a | b)),
            (Err(e), _) | (_, Err(e)) => Err(e).wrap_err("during bitwise or"),
        }
    }
}

impl ops::BitXor for LuaNumber {
    type Output = crate::Result<LuaNumber>;

    fn bitxor(self, other: Self) -> crate::Result<LuaNumber> {
        match (self.integer_repr(), other.integer_repr()) {
            (Ok(a), Ok(b)) => Ok(LuaNumber::Integer(a ^ b)),
            (Err(e), _) | (_, Err(e)) => Err(e).wrap_err("during bitwise xor"),
        }
    }
}

impl ops::BitXor for &LuaNumber {
    type Output = crate::Result<LuaNumber>;

    fn bitxor(self, other: Self) -> crate::Result<LuaNumber> {
        match (self.integer_repr(), other.integer_repr()) {
            (Ok(a), Ok(b)) => Ok(LuaNumber::Integer(a ^ b)),
            (Err(e), _) | (_, Err(e)) => Err(e).wrap_err("during bitwise xor"),
        }
    }
}

impl ops::Shl for &LuaNumber {
    type Output = crate::Result<LuaNumber>;

    fn shl(self, other: Self) -> crate::Result<LuaNumber> {
        match (self.integer_repr(), other.integer_repr()) {
            (Ok(a), Ok(b)) => Ok(LuaNumber::Integer(if b >= 0 {
                a.checked_shl(b as u32).unwrap_or(0)
            } else {
                a.checked_shr(-b as u32).unwrap_or(0)
            })),
            (Err(e), _) | (_, Err(e)) => Err(e).wrap_err("during bitwise shift left"),
        }
    }
}

impl ops::Shr for &LuaNumber {
    type Output = crate::Result<LuaNumber>;

    fn shr(self, other: Self) -> crate::Result<LuaNumber> {
        match (self.integer_repr(), other.integer_repr()) {
            (Ok(a), Ok(b)) => Ok(LuaNumber::Integer(if b >= 0 {
                a.checked_shr(b as u32).unwrap_or(0)
            } else {
                a.checked_shl(-b as u32).unwrap_or(0)
            })),
            (Err(e), _) | (_, Err(e)) => Err(e).wrap_err("during bitwise shift right"),
        }
    }
}

impl ops::Not for &LuaNumber {
    type Output = crate::Result<LuaNumber>;

    fn not(self) -> crate::Result<LuaNumber> {
        match self.integer_repr() {
            Ok(a) => Ok(LuaNumber::Integer(!a)),
            Err(e) => Err(e).wrap_err("during bitwise not"),
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
    pub fn pow(&self, other: &Self) -> Self {
        match (self, other) {
            (LuaNumber::Integer(a), LuaNumber::Integer(b)) => {
                LuaNumber::Integer(a.wrapping_pow(*b as u32))
            }
            (LuaNumber::Float(a), LuaNumber::Float(b)) => LuaNumber::Float(a.powf(*b)),
            (LuaNumber::Integer(a), LuaNumber::Float(b)) => LuaNumber::Float((*a as f64).powf(*b)),
            (LuaNumber::Float(a), LuaNumber::Integer(b)) => LuaNumber::Float(a.powi(*b as i32)),
        }
    }

    pub fn idiv(self, other: Self) -> Self {
        (self / other).floor()
    }

    pub fn floor(self) -> Self {
        match self {
            LuaNumber::Integer(a) => LuaNumber::Integer(a),
            LuaNumber::Float(a) => LuaNumber::Float(a.floor()),
        }
    }
}
