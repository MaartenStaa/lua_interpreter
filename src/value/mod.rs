mod attribute;
pub(crate) mod callable;
mod closure;
mod constant;
mod conversions;
mod function;
pub(crate) mod metatables;
mod number;
mod object;
pub(crate) mod string;
mod table;
mod upvalue;
mod userdata;

pub use attribute::LuaVariableAttribute;
use callable::{Callable, Method};
pub use closure::LuaClosure;
pub use constant::LuaConst;
pub use function::NativeLuaFunction;
pub use number::{IntegerRepr, LuaNumber};
pub use object::LuaObject;
pub use string::LuaString;
pub use table::LuaTable;
pub use upvalue::UpValue;
pub use userdata::UserData;

use std::{
    borrow::Borrow,
    fmt::{Debug, Display},
    hash::Hash,
    sync::{Arc, RwLock},
};

use crate::{LuaState, error::lua_error};

/// Is it a bird? Is it a plane? No, it's a [`LuaValue`]! But maybe it's a
/// string in a trench coat, or a boolean.
///
/// # Layout
///
/// An IEEE 754 `f64` looks like:
///
/// ```text
/// [S][EEEEEEEEEEE][MMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMMM]
///  1     11 bits                        52 bits
/// MSB                                                                 LSB
/// ```
///
/// The trench coat is an f64, using NaN boxing to store all Lua values in a
/// single 64-bit word. Floats support many different values of `NaN`, one
/// of which is reserved to represent Lua values. A `NaN` requires that all `E`
/// bits are `1`, and `M != 0`. We can use the top `M` bits to store a type
/// tag, and the remaining `M` bits to store the value. This allows us to store
/// all Lua values in a single 64-bit word, without needing to heap allocate
/// for small values like numbers and booleans.
///
/// For larger values like strings and tables, we can store a pointer to the
/// heap allocated value in the remaining `M` bits.
///
/// We set the top 12 bits (`S` and `E`) to all ones. Combined with a non-zero
/// type tag in the top 3 `M` bits, this guarantees a valid `f64` `NaN`.
///
/// Tagged values have this layout:
///
/// ```text
/// [SSEEEEEEEEEE][TTT][VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV]
///  111111111111  tag              49-bit value
///  (12 bits)   (3 bits)
/// ```
///
/// # Assumption: negative NaN
///
/// This scheme assumes that hardware float operations produce positive NaN
/// values (sign bit = 0), as is the case on x86-64 and aarch64. A negative
/// NaN produced by hardware could collide with our tagged value space. If this
/// becomes a problem, float results must be canonicalized after each operation.
///
/// # Types
///
/// ## Floats
///
/// Any value where `S` and `E` are not all ones, or `M` is all zeroes
/// (the latter being negative infinity, a valid float).
///
/// ## Booleans (tag `001`)
///
/// `true` is stored in the bottom bit as `1`, `false` has all value bits
/// set to `0`.
///
/// ```text
/// [SSEEEEEEEEEE][TTT][VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV]
///  111111111111  001   \----------- unused (48 bits) ----------/       \B/
/// ```
///
/// ## Nil (tag `010`)
///
/// All value bits are unused, as there is only one valid `nil` value.
///
/// ```text
/// [SSEEEEEEEEEE][TTT][VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV]
///  111111111111  010   \-------------- unused (49 bits) ---------------/
/// ```
///
/// ## Integers (tags `011` and `100`)
///
/// Since Lua integers use the full `i64` range, two type tags are used:
///
/// **Small integers** (tag `011`): the value is stored inline as a
/// sign-extended 49-bit integer (range ±2^48, approximately ±281 trillion).
///
/// ```text
/// [SSEEEEEEEEEE][TTT][VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV]
///  111111111111  011   \----------- 49-bit signed integer -------------/
/// ```
///
/// **Large integers** (tag `100`): the 49 value bits contain a pointer to
/// a heap-allocated `Box<i64>`.
///
/// ```text
/// [SSEEEEEEEEEE][TTT][VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV]
///  111111111111  100   \------------ pointer to Box<i64> --------------/
/// ```
///
/// ## Strings (tag `101`)
///
/// Stores a pointer to a heap-allocated string value in the 49 value bits.
///
/// ```text
/// [SSEEEEEEEEEE][TTT][VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV]
///  111111111111  101   \------------- pointer to string ---------------/
/// ```
///
/// ## Objects (tag `110`)
///
/// Stores a pointer to a `Arc<RwLock<LuaObject>>` in the 49 value bits.
///
/// ```text
/// [SSEEEEEEEEEE][TTT][VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV]
///  111111111111  110   \------ pointer to Arc<RwLock<LuaObject>> ------/
/// ```
///
/// ## Upvalues (tag `111`)
///
/// Stores a pointer to a `Arc<RwLock<UpValue>>` in the 49 value bits.
///
/// ```text
/// [SSEEEEEEEEEE][TTT][VVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVVV]
///  111111111111  111   \------ pointer to Arc<RwLock<UpValue>> --------/
/// ```
#[repr(transparent)]
pub struct LuaValue(u64);

#[derive(Clone)]
pub enum OldLuaValue {
    Nil,                       // 000
    Boolean(bool),             // 010
    Number(number::LuaNumber), //
    String(LuaString),
    Object(Arc<RwLock<LuaObject>>),
    UpValue(Arc<RwLock<UpValue>>),
}

#[repr(u8)]
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum LuaValueType {
    Float = 0,
    Boolean = 1,
    Nil = 2,
    SmallInteger = 3,
    LargeInteger = 4,
    String = 5,
    Object = 6,
    UpValue = 7,
}

impl LuaValue {
    /// Top 12 bits (sign + exponent) all set to 1. When combined with a
    /// non-zero mantissa, this produces a NaN — which we repurpose to
    /// encode non-float Lua values.
    const MARKER: u64 = 0xFFF0_0000_0000_0000;

    /// Mask for the mantissa (bottom 52 bits).
    const MANTISSA_MASK: u64 = 0x000F_FFFF_FFFF_FFFF;

    /// Mask for the 49-bit value portion (mantissa minus the 3-bit type tag).
    const VALUE_MASK: u64 = 0x0001_FFFF_FFFF_FFFF;

    /// Bit position of the 3-bit type tag within the 52-bit mantissa.
    const TYPE_TAG_SHIFT: u32 = 49;

    /// Maximum value for a small (inline) integer: 2^48 - 1.
    const SMALL_INT_MAX: i64 = (1_i64 << 48) - 1;

    /// Minimum value for a small (inline) integer: -2^48.
    const SMALL_INT_MIN: i64 = -(1_i64 << 48);

    /// Construct a tagged NaN-boxed value from a type tag (1-7) and a 49-bit payload.
    #[inline(always)]
    const fn tagged(tag: u8, value: u64) -> Self {
        // debug_assert!(tag >= 1 && tag <= 7, "type tag must be 1-7, got {tag}");
        // debug_assert!(
        //     value & !Self::VALUE_MASK == 0,
        //     "value doesn't fit in 49 bits: {value:#x}"
        // );
        Self(Self::MARKER | ((tag as u64) << Self::TYPE_TAG_SHIFT) | value)
    }

    pub const NIL: Self = Self::tagged(2, 0);

    #[must_use]
    pub fn from_bool(b: bool) -> Self {
        Self::tagged(1, u64::from(b))
    }

    #[must_use]
    pub fn from_float(f: f64) -> Self {
        Self(f.to_bits())
    }

    #[must_use]
    pub fn from_integer(i: i64) -> Self {
        if (Self::SMALL_INT_MIN..=Self::SMALL_INT_MAX).contains(&i) {
            Self::tagged(3, i.cast_unsigned() & Self::VALUE_MASK)
        } else {
            let ptr = Box::into_raw(Box::new(i));
            debug_assert!(
                ptr as u64 & !Self::VALUE_MASK == 0,
                "Box<i64> pointer doesn't fit in 49 bits"
            );
            Self::tagged(4, ptr as u64)
        }
    }

    #[must_use]
    pub fn from_number(n: impl Borrow<LuaNumber>) -> Self {
        match n.borrow() {
            LuaNumber::Integer(i) => Self::from_integer(*i),
            LuaNumber::Float(f) => Self::from_float(*f),
        }
    }

    pub fn from_string(s: impl Into<LuaString>) -> Self {
        let ptr = Box::into_raw(Box::new(s.into()));
        debug_assert!(
            ptr as u64 & !Self::VALUE_MASK == 0,
            "Box<LuaString> pointer doesn't fit in 49 bits"
        );
        Self::tagged(5, ptr as u64)
    }

    pub fn from_object(obj: Arc<RwLock<LuaObject>>) -> Self {
        let ptr = Arc::into_raw(obj);
        debug_assert!(
            ptr as u64 & !Self::VALUE_MASK == 0,
            "Arc<RwLock<LuaObject>> pointer doesn't fit in 49 bits"
        );
        Self::tagged(6, ptr as u64)
    }

    pub fn from_upvalue(uv: Arc<RwLock<UpValue>>) -> Self {
        let ptr = Arc::into_raw(uv);
        debug_assert!(
            ptr as u64 & !Self::VALUE_MASK == 0,
            "Arc<RwLock<UpValue>> pointer doesn't fit in 49 bits"
        );
        Self::tagged(7, ptr as u64)
    }

    /// Returns `true` if this value is a NaN-boxed tagged value (not a float).
    /// All marker bits must be set AND the mantissa must be non-zero
    /// (all-zero mantissa with marker set is negative infinity, a valid float).
    #[inline(always)]
    fn is_tagged(&self) -> bool {
        (self.0 & Self::MARKER) == Self::MARKER && (self.0 & Self::MANTISSA_MASK) != 0
    }

    /// Extract the 3-bit type tag from the mantissa (bits 51-49).
    #[inline(always)]
    fn get_type_tag(&self) -> LuaValueType {
        unsafe { std::mem::transmute(((self.0 >> Self::TYPE_TAG_SHIFT) & 0x7) as u8) }
    }

    /// Extract the 49-bit value portion (bits 48-0).
    #[inline(always)]
    fn get_value_bits(&self) -> u64 {
        self.0 & Self::VALUE_MASK
    }

    /// Get the type of this value.
    #[must_use]
    pub fn get_type(&self) -> LuaValueType {
        if !self.is_tagged() {
            return LuaValueType::Float;
        }

        self.get_type_tag()
    }

    /// Convert to the old enum representation for migration purposes.
    ///
    /// Safe to call multiple times: pointer-based types have their refcounts
    /// incremented (Arc) or values copied by read (Box<i64>), so no ownership
    /// is stolen from the NaN-boxed representation.
    #[must_use]
    pub fn as_value(&self) -> OldLuaValue {
        if !self.is_tagged() {
            return OldLuaValue::Number(LuaNumber::Float(f64::from_bits(self.0)));
        }

        let value = self.get_value_bits();

        match self.get_type_tag() {
            LuaValueType::Boolean => OldLuaValue::Boolean(value & 1 == 1),
            LuaValueType::Nil => OldLuaValue::Nil,
            LuaValueType::SmallInteger => {
                // Sign-extend from 49 bits to 64 bits
                let extended = (value.cast_signed() << 15) >> 15;
                OldLuaValue::Number(LuaNumber::Integer(extended))
            }
            LuaValueType::LargeInteger => {
                let ptr = value as *const i64;
                OldLuaValue::Number(LuaNumber::Integer(unsafe { *ptr }))
            }
            LuaValueType::String => {
                let ptr = value as *const LuaString;
                OldLuaValue::String(unsafe { (*ptr).clone() })
            }
            LuaValueType::Object => {
                let ptr = value as *const RwLock<LuaObject>;
                unsafe { Arc::increment_strong_count(ptr) };
                OldLuaValue::Object(unsafe { Arc::from_raw(ptr) })
            }
            LuaValueType::UpValue => {
                let ptr = value as *const RwLock<UpValue>;
                unsafe { Arc::increment_strong_count(ptr) };
                OldLuaValue::UpValue(unsafe { Arc::from_raw(ptr) })
            }
            LuaValueType::Float => unreachable!("invalid type tag"),
        }
    }

    // -- Type checks --

    #[must_use]
    pub fn is_nil(&self) -> bool {
        self.is_tagged() && self.get_type_tag() == LuaValueType::Nil
    }

    #[must_use]
    pub fn is_bool(&self) -> bool {
        self.is_tagged() && self.get_type_tag() == LuaValueType::Boolean
    }

    #[must_use]
    pub fn is_float(&self) -> bool {
        !self.is_tagged()
    }

    #[must_use]
    pub fn is_integer(&self) -> bool {
        self.is_tagged()
            && matches!(
                self.get_type_tag(),
                LuaValueType::SmallInteger | LuaValueType::LargeInteger
            )
    }

    #[must_use]
    pub fn is_number(&self) -> bool {
        self.is_float() || self.is_integer()
    }

    #[must_use]
    pub fn is_string(&self) -> bool {
        self.is_tagged() && self.get_type_tag() == LuaValueType::String
    }

    #[must_use]
    pub fn is_object(&self) -> bool {
        self.is_tagged() && self.get_type_tag() == LuaValueType::Object
    }

    #[must_use]
    pub fn is_upvalue(&self) -> bool {
        self.is_tagged() && self.get_type_tag() == LuaValueType::UpValue
    }

    // -- Accessors --

    // All `as_*` methods are unsafe: the caller must ensure the value is
    // actually the expected type. Debug builds assert this; release builds
    // assume it.

    /// # Safety
    /// Value must be a float (not a tagged NaN-boxed value).
    #[must_use]
    pub unsafe fn as_float(&self) -> f64 {
        debug_assert!(self.is_float(), "not a float");
        f64::from_bits(self.0)
    }

    /// # Safety
    /// Value must be a boolean.
    #[must_use]
    pub unsafe fn as_bool(&self) -> bool {
        debug_assert!(self.is_bool(), "not a boolean");
        self.get_value_bits() & 1 == 1
    }

    /// # Safety
    /// Value must be an integer (small or large).
    #[must_use]
    pub unsafe fn as_integer(&self) -> i64 {
        debug_assert!(self.is_integer(), "not an integer");
        let value = self.get_value_bits();
        if self.get_type_tag() == LuaValueType::SmallInteger {
            (value.cast_signed() << 15) >> 15
        } else {
            unsafe { *(value as *const i64) }
        }
    }

    /// # Safety
    /// Value must be a number (float or integer).
    #[must_use]
    pub unsafe fn as_number(&self) -> LuaNumber {
        debug_assert!(self.is_number(), "not a number");
        if self.is_float() {
            LuaNumber::Float(f64::from_bits(self.0))
        } else {
            LuaNumber::Integer(unsafe { self.as_integer() })
        }
    }

    /// # Safety
    /// Value must be a string.
    #[must_use]
    pub unsafe fn as_string(&self) -> &LuaString {
        debug_assert!(self.is_string(), "not a string");
        unsafe { &*(self.get_value_bits() as *const LuaString) }
    }

    /// # Safety
    /// Value must be a string.
    #[must_use]
    pub unsafe fn into_string(self) -> LuaString {
        debug_assert!(self.is_string(), "not a string");
        let s = *unsafe { Box::from_raw(self.get_value_bits() as *mut LuaString) };
        std::mem::forget(self);
        s
    }

    /// # Safety
    /// Value must be an object.
    #[must_use]
    pub unsafe fn as_object(&self) -> &RwLock<LuaObject> {
        debug_assert!(self.is_object(), "not an object");
        unsafe { &*(self.get_value_bits() as *const RwLock<LuaObject>) }
    }

    /// # Safety
    /// Value must be an object.
    #[must_use]
    pub unsafe fn into_object(&self) -> Arc<RwLock<LuaObject>> {
        debug_assert!(self.is_object(), "not an object");
        let ptr = self.get_value_bits() as *const RwLock<LuaObject>;
        unsafe { Arc::increment_strong_count(ptr) };
        unsafe { Arc::from_raw(ptr) }
    }

    /// # Safety
    /// Value must be an upvalue.
    #[must_use]
    pub unsafe fn as_upvalue(&self) -> &RwLock<UpValue> {
        debug_assert!(self.is_upvalue(), "not an upvalue");
        unsafe { &*(self.get_value_bits() as *const RwLock<UpValue>) }
    }

    /// # Safety
    /// Value must be an upvalue.
    #[must_use]
    pub unsafe fn into_upvalue(&self) -> Arc<RwLock<UpValue>> {
        debug_assert!(self.is_upvalue(), "not an upvalue");
        let ptr = self.get_value_bits() as *const RwLock<UpValue>;
        unsafe { Arc::increment_strong_count(ptr) };
        unsafe { Arc::from_raw(ptr) }
    }
}

impl Drop for LuaValue {
    fn drop(&mut self) {
        if !self.is_tagged() {
            return;
        }
        let value = self.get_value_bits();
        match self.get_type_tag() {
            LuaValueType::LargeInteger => {
                drop(unsafe { Box::from_raw(value as *mut i64) });
            }
            LuaValueType::String => {
                drop(unsafe { Box::from_raw(value as *mut LuaString) });
            }
            LuaValueType::Object => {
                unsafe { Arc::decrement_strong_count(value as *const RwLock<LuaObject>) };
            }
            LuaValueType::UpValue => {
                unsafe { Arc::decrement_strong_count(value as *const RwLock<UpValue>) };
            }
            _ => {}
        }
    }
}

impl LuaValue {
    /// Turn the value into a string, using the `__tostring` metamethod if it exists.
    /// Falls back on this type's `Display` implementation if the metamethod does not exist.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    ///
    /// - The `__tostring` metamethod exists but is not a function.
    /// - The `__tostring` metamethod returns an error.
    /// - The `__tostring` metamethod returns a value that is not a string.
    pub fn cast_to_string(&self, state: &mut LuaState) -> crate::Result<LuaString> {
        let __tostring = self.get_metavalue(&metatables::TOSTRING_KEY);
        if let Some(__tostring) = __tostring {
            let __tostring: Callable = (&__tostring).try_into().map_err(|_| {
                lua_error!(
                    "invalid value for __tostring metamethod in {}, expected function, got {}",
                    self.type_name(),
                    __tostring.type_name()
                )
            })?;

            let result = match __tostring.method {
                Method::Closure(closure) => {
                    let args_r = state.extend_stack(LuaValue::NIL, [self.clone()]);
                    state
                        .run_closure(&closure, args_r, 1, 2)?
                        .unwrap_completed()
                        .into_iter()
                        .next()
                }
                nf @ Method::NativeFunction(name, f) => {
                    let args_r = state.extend_stack(nf, [self.clone()]);
                    let num_return = state.run_native_function(name, f, args_r, 1, 2)?;
                    debug_assert!(
                        num_return >= 1,
                        "native function {name} did not return a value",
                    );
                    Some(state.get(args_r))
                }
            };

            return match result.unwrap_or(LuaValue::NIL) {
                v if v.is_string() => Ok(unsafe { v.into_string() }),
                v => Err(lua_error!(
                    "__tostring metamethod must return a string, got {}",
                    v.type_name()
                )),
            };
        }

        Ok(format!("{self}").into_bytes().into())
    }
}

impl Debug for LuaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.as_value() {
            OldLuaValue::Nil => write!(f, "nil"),
            OldLuaValue::Boolean(b) => write!(f, "{b}"),
            OldLuaValue::Number(n) => write!(f, "{n}"),
            OldLuaValue::String(s) => write!(f, "{:?}", String::from_utf8_lossy(&s)),
            // NOTE: Here and for UpValue, lock by write to prevent deadlock/stack overflow, in the
            // case where values are self-referential.
            OldLuaValue::Object(o) => match o.try_write() {
                Ok(o) => {
                    f.debug_struct("object").field("0", &o).finish()
                    // write!(f, "{o:?}")
                }
                Err(e) => {
                    write!(f, "object<{o:012p}>: <locked: {e}>")
                }
            },
            OldLuaValue::UpValue(u) => match u.try_write() {
                Ok(inner) => write!(f, "{inner:?}"),
                Err(e) => {
                    write!(f, "upvalue<{u:012p}>: <locked: {e}>")
                }
            },
        }
    }
}

impl Clone for LuaValue {
    fn clone(&self) -> Self {
        if !self.is_tagged() {
            return Self(self.0);
        }

        match self.get_type_tag() {
            LuaValueType::Boolean | LuaValueType::Nil | LuaValueType::SmallInteger => Self(self.0),
            LuaValueType::LargeInteger => {
                let ptr = self.get_value_bits() as *const i64;
                let value = unsafe { *ptr };
                Self::from_integer(value)
            }
            LuaValueType::String => {
                let ptr = self.get_value_bits() as *const LuaString;
                let value = unsafe { (*ptr).clone() };
                Self::from_string(value)
            }
            LuaValueType::Object => {
                let ptr = self.get_value_bits() as *const RwLock<LuaObject>;
                unsafe { Arc::increment_strong_count(ptr) };
                Self::from_object(unsafe { Arc::from_raw(ptr) })
            }
            LuaValueType::UpValue => {
                let ptr = self.get_value_bits() as *const RwLock<UpValue>;
                unsafe { Arc::increment_strong_count(ptr) };
                Self::from_upvalue(unsafe { Arc::from_raw(ptr) })
            }
            LuaValueType::Float => unreachable!("invalid type tag"),
        }
    }
}

impl PartialEq for LuaValue {
    fn eq(&self, other: &Self) -> bool {
        let lhs = if self.is_upvalue() {
            &*unsafe { self.as_upvalue() }.read().unwrap()
        } else {
            self
        };
        let rhs = if other.is_upvalue() {
            &*unsafe { other.as_upvalue() }.read().unwrap()
        } else {
            other
        };

        // Safety: We verify the type of each value before extracting the value itself unsafely.
        unsafe {
            match (lhs.get_type(), rhs.get_type()) {
                (LuaValueType::Nil, LuaValueType::Nil) => true,
                (LuaValueType::Boolean, LuaValueType::Boolean) => lhs.as_bool() == rhs.as_bool(),
                (
                    LuaValueType::Float | LuaValueType::SmallInteger | LuaValueType::LargeInteger,
                    LuaValueType::Float | LuaValueType::SmallInteger | LuaValueType::LargeInteger,
                ) => lhs.as_number() == rhs.as_number(),
                (LuaValueType::String, LuaValueType::String) => lhs.as_string() == rhs.as_string(),
                (LuaValueType::Object, LuaValueType::Object) => {
                    *lhs.as_object().read().unwrap() == *rhs.as_object().read().unwrap()
                }
                // NOTE: Don't need to account for upvalues here since they are dereferenced above.
                _ => false,
            }
        }
    }
}

impl Eq for LuaValue {}

impl Hash for LuaValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Safety: We verify the type of each value before extracting the value itself unsafely.
        unsafe {
            match self.get_type() {
                LuaValueType::Nil => {
                    state.write_u8(0);
                }
                LuaValueType::Boolean => {
                    state.write_u8(1);
                    state.write_u8(u8::from(self.as_bool()));
                }
                LuaValueType::Float | LuaValueType::SmallInteger | LuaValueType::LargeInteger => {
                    state.write_u8(2);
                    match self.get_type() {
                        LuaValueType::Float => state.write_u64(self.as_float().to_bits()),
                        _ => state.write_i64(self.as_integer()),
                    }
                }
                LuaValueType::String => {
                    state.write_u8(3);
                    self.as_string().hash(state);
                }
                LuaValueType::Object => {
                    state.write_u8(4);
                    self.as_object().read().unwrap().hash(state);
                }
                LuaValueType::UpValue => {
                    state.write_u8(5);
                    self.as_upvalue().read().unwrap().hash(state);
                }
            }
        }
    }
}

impl LuaValue {
    pub const MAX_STRING_LENGTH: usize = const {
        if size_of::<usize>() < size_of::<i32>() {
            usize::MAX
        } else {
            i32::MAX as usize
        }
    };

    /// Returns the size of this value in bytes, including any heap allocations.
    ///
    /// # Panics
    ///
    /// Panics when failing to acquire a read lock on an object or upvalue.
    #[must_use]
    pub fn size_bytes(&self) -> u64 {
        (size_of::<Self>() as u64)
            + match self.get_type() {
                LuaValueType::String => size_of_val(unsafe { self.as_string() }) as u64,
                LuaValueType::Object => {
                    let o = unsafe { self.as_object() };
                    let o = o.read().unwrap();
                    match &*o {
                        LuaObject::Table(t) => t.size_bytes(),
                        // TODO: Once we have threads or userdata, this probably needs to change.
                        _ => 0,
                    }
                }
                _ => 0,
            }
    }

    /// Get the type name of this value as a string, e.g. "nil", "boolean", "number", "string", "table", etc.
    ///
    /// # Panics
    ///
    /// Panics when failing to acquire a read lock on an object or upvalue.
    #[must_use]
    pub fn type_name(&self) -> &'static str {
        match self.get_type() {
            LuaValueType::Nil => "nil",
            LuaValueType::Boolean => "boolean",
            LuaValueType::Float | LuaValueType::SmallInteger | LuaValueType::LargeInteger => {
                "number"
            }
            LuaValueType::String => "string",
            LuaValueType::Object => unsafe { self.as_object() }.read().unwrap().type_name(),
            LuaValueType::UpValue => unsafe { self.as_upvalue() }.read().unwrap().type_name(),
        }
    }
}

impl Display for LuaValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.get_type() {
            LuaValueType::Nil => write!(f, "nil"),
            LuaValueType::Boolean => write!(f, "{}", unsafe { self.as_bool() }),
            LuaValueType::Float | LuaValueType::SmallInteger | LuaValueType::LargeInteger => {
                write!(f, "{}", unsafe { self.as_number() })
            }
            LuaValueType::String => write!(
                f,
                "{}",
                String::from_utf8_lossy(unsafe { self.as_string() })
            ),
            LuaValueType::Object => write!(f, "{}", unsafe { self.as_object() }.read().unwrap()),
            LuaValueType::UpValue => {
                write!(f, "{}", unsafe { self.as_upvalue() }.read().unwrap())
            }
        }
    }
}

impl LuaValue {
    /// Checks if the value is a table. Returns `false` for non-object values.
    ///
    /// # Panics
    ///
    /// Panics when failing to acquire a read lock on an object.
    #[must_use]
    pub fn is_table(&self) -> bool {
        if !self.is_object() {
            return false;
        }

        unsafe { self.as_object() }.read().unwrap().is_table()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_encode_values() {
        let nil = LuaValue::NIL;
        assert!(nil.is_nil());

        let b_true = LuaValue::from_bool(true);
        assert!(b_true.is_bool());
        assert!(unsafe { b_true.as_bool() });

        let b_false = LuaValue::from_bool(false);
        assert!(b_false.is_bool());
        assert!(!unsafe { b_false.as_bool() });

        let float = LuaValue::from_float(12.51989);
        assert!(float.is_float());

        let small_int = LuaValue::from_integer(42);
        assert!(small_int.is_integer());
        assert_eq!(unsafe { small_int.as_integer() }, 42);

        let small_neg_int = LuaValue::from_integer(-42);
        assert!(small_neg_int.is_integer());
        assert_eq!(unsafe { small_neg_int.as_integer() }, -42);

        let large_int = LuaValue::from_integer(1 << 50);
        assert!(large_int.is_integer());
        assert_eq!(unsafe { large_int.as_integer() }, 1 << 50);

        let string = LuaValue::from_string(LuaString::from(b"hello"));
        assert!(string.is_string());
        assert_eq!(unsafe { string.as_string() }, b"hello");

        let object =
            LuaValue::from_object(Arc::new(RwLock::new(LuaObject::Table(LuaTable::new()))));
        assert!(object.is_object());
        assert!(unsafe { object.as_object() }.read().unwrap().is_table());

        let upvalue =
            LuaValue::from_upvalue(Arc::new(RwLock::new(UpValue(LuaValue::from_integer(16)))));
        assert!(upvalue.is_upvalue());
        assert!(unsafe { upvalue.as_upvalue() }.read().unwrap().is_integer());

        let all = [
            &nil, &b_true, &b_false, &float, &small_int, &large_int, &string, &object, &upvalue,
        ];
        for value in all {
            let cloned = value.clone();
            assert_eq!(value, &cloned);
        }

        let all_except_float = [
            &nil, &b_true, &b_false, &small_int, &large_int, &string, &object, &upvalue,
        ];
        for value in all_except_float {
            let as_float = f64::from_bits(value.0);
            assert!(
                as_float.is_nan(),
                "expected NaN for non-float value, got {as_float}",
            );
        }
    }
}
