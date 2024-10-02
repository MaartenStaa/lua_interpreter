use miette::miette;

pub trait ByteCharExt {
    fn to_digit(self, radix: u32) -> Option<u8>;
}

impl ByteCharExt for u8 {
    fn to_digit(self, radix: u32) -> Option<u8> {
        if radix > 36 {
            return None;
        }

        if self.is_ascii_digit() {
            Some(self - b'0')
        } else if self.is_ascii_alphabetic() {
            let offset = if self.is_ascii_lowercase() {
                b'a'
            } else {
                b'A'
            };
            Some(10 + self - offset)
        } else {
            None
        }
    }
}

pub trait ByteSliceExt
where
    Self: Sized,
{
    fn from_bytes_radix(slice: &[u8], radix: u32) -> miette::Result<Self>;
}

impl ByteSliceExt for i64 {
    fn from_bytes_radix(slice: &[u8], radix: u32) -> miette::Result<Self> {
        let mut result = 0i64;
        for &byte in slice {
            let digit = byte
                .to_digit(radix)
                .ok_or_else(|| miette!("invalid digit '{:0x}", byte))?;
            result = result * radix as i64 + digit as i64;
        }

        Ok(result)
    }
}
