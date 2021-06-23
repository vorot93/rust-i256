#![no_std]
#![doc = include_str!("../README.md")]

use core::cmp::Ordering;
use ethnum::U256;

#[derive(Copy, Clone, Eq, PartialEq, Debug)]
pub enum Sign {
    Plus,
    Minus,
    Zero,
}

pub const SIGN_BIT_MASK: U256 = U256::from_words(
    FLIPH_BITMASK_U128,
    0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF,
);

pub const MIN_NEGATIVE_VALUE: U256 = U256::from_words(SIGN_BITMASK_U128, 0);

const SIGN_BITMASK_U128: u128 = 0x8000_0000_0000_0000_0000_0000_0000_0000;
const FLIPH_BITMASK_U128: u128 = 0x7FFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF;

#[inline(always)]
pub fn i256_sign<const DO_TWO_COMPL: bool>(val: &mut U256) -> Sign {
    if val.high() & SIGN_BITMASK_U128 == 0 {
        if *val == U256::ZERO {
            Sign::Zero
        } else {
            Sign::Plus
        }
    } else {
        if DO_TWO_COMPL {
            two_compl_mut(val);
        }
        Sign::Minus
    }
}

#[inline(always)]
fn u256_remove_sign(val: &mut U256) {
    *val.high_mut() &= FLIPH_BITMASK_U128;
}

#[inline(always)]
pub fn two_compl_mut(op: &mut U256) {
    *op = two_compl(*op);
}

pub fn two_compl(op: U256) -> U256 {
    !op + U256::ONE
}

#[inline(always)]
pub fn i256_cmp(mut first: U256, mut second: U256) -> Ordering {
    let first_sign = i256_sign::<false>(&mut first);
    let second_sign = i256_sign::<false>(&mut second);
    match (first_sign, second_sign) {
        (Sign::Zero, Sign::Zero) => Ordering::Equal,
        (Sign::Zero, Sign::Plus) => Ordering::Less,
        (Sign::Zero, Sign::Minus) => Ordering::Greater,
        (Sign::Minus, Sign::Zero) => Ordering::Less,
        (Sign::Minus, Sign::Plus) => Ordering::Less,
        (Sign::Minus, Sign::Minus) => first.cmp(&second),
        (Sign::Plus, Sign::Minus) => Ordering::Greater,
        (Sign::Plus, Sign::Zero) => Ordering::Greater,
        (Sign::Plus, Sign::Plus) => first.cmp(&second),
    }
}

#[inline(always)]
pub fn i256_div(mut first: U256, mut second: U256) -> U256 {
    let second_sign = i256_sign::<true>(&mut second);
    if second_sign == Sign::Zero {
        return U256::ZERO;
    }
    let first_sign = i256_sign::<true>(&mut first);
    if first_sign == Sign::Minus && first == MIN_NEGATIVE_VALUE && second == U256::ONE {
        return two_compl(MIN_NEGATIVE_VALUE);
    }

    let mut d = first / second;

    u256_remove_sign(&mut d);

    if d == U256::ZERO {
        return U256::ZERO;
    }

    match (first_sign, second_sign) {
        (Sign::Zero, Sign::Plus)
        | (Sign::Plus, Sign::Zero)
        | (Sign::Zero, Sign::Zero)
        | (Sign::Plus, Sign::Plus)
        | (Sign::Minus, Sign::Minus) => d,
        (Sign::Zero, Sign::Minus)
        | (Sign::Plus, Sign::Minus)
        | (Sign::Minus, Sign::Zero)
        | (Sign::Minus, Sign::Plus) => two_compl(d),
    }
}

#[inline(always)]
pub fn i256_mod(mut first: U256, mut second: U256) -> U256 {
    let first_sign = i256_sign::<true>(&mut first);
    if first_sign == Sign::Zero {
        return U256::ZERO;
    }

    let _ = i256_sign::<true>(&mut second);
    let mut r = first % second;
    u256_remove_sign(&mut r);
    if r == U256::ZERO {
        return U256::ZERO;
    }
    if first_sign == Sign::Minus {
        two_compl(r)
    } else {
        r
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use core::num::Wrapping;
    use ethnum::*;

    #[test]
    fn div_i256() {
        assert_eq!(Wrapping(i8::MIN) / Wrapping(-1), Wrapping(i8::MIN));
        assert_eq!(i8::MAX / -1, -i8::MAX);

        let one = 1.as_u256();
        let one_hundred = 100.as_u256();
        let fifty = 50.as_u256();
        let _fifty_sign = Sign::Plus;
        let two = 2.as_u256();
        let neg_one_hundred = 100.as_u256();
        let _neg_one_hundred_sign = Sign::Minus;
        let minus_one = 1.as_u256();
        let max_value = 2.as_u256().pow(255) - 1;
        let neg_max_value = 2.as_u256().pow(255) - 1;

        assert_eq!(i256_div(MIN_NEGATIVE_VALUE, minus_one), MIN_NEGATIVE_VALUE);
        assert_eq!(i256_div(MIN_NEGATIVE_VALUE, one), MIN_NEGATIVE_VALUE);
        assert_eq!(i256_div(max_value, one), max_value);
        assert_eq!(i256_div(max_value, minus_one), neg_max_value);
        assert_eq!(i256_div(one_hundred, minus_one), neg_one_hundred);
        assert_eq!(i256_div(one_hundred, two), fifty);
    }
}
