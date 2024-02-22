use num_bigint::{BigInt, BigUint, ToBigInt, ToBigUint};
use crate::core::mics_func::extended_euclid;

pub trait Ring {
    type Set;

    fn add(&self, a : &Self::Set, b : &Self::Set) -> Self::Set;
    fn add_inv(&self, a : &Self::Set) -> Self::Set;
    fn mul(&self, a : &Self::Set, b :  &Self::Set) -> Self::Set;
}

pub trait Field : Ring {
    fn mul_inv(&self, a : &Self::Set) -> Self::Set;
}

#[derive(Debug, PartialEq)]
pub struct ZZ {
    //elem : ElemType  //this should be BigInt, also should impl Display
}

impl ZZ {
    pub fn new() -> ZZ  //should be generic or generated for all num types
    {
        ZZ {}
    }
}

impl Ring for ZZ {

    type Set = BigInt;

    fn add(&self, a : &Self::Set, b :  &Self::Set) -> Self::Set {
        a.clone() + b.clone()
    }

    fn add_inv(&self, a : &Self::Set) -> Self::Set {
        - a.clone()
    }

    fn mul(&self, a : &Self::Set, b :  &Self::Set) -> Self::Set {
        a.clone() * b.clone()
    }
}

#[allow(non_camel_case_types)]
#[derive(Debug)]
pub struct ZZ_n<'a> {  
    modulo : &'a BigUint
}

impl<'a> ZZ_n<'a>{

    pub fn new(m : &'a BigUint) -> ZZ_n<'a> 
    {
        ZZ_n {modulo : m}
    }

    fn _mod(&self, b : &BigUint) -> BigUint
    {
        let one = 1.to_biguint().unwrap();
        b.modpow(&one, self.modulo)
    }

    fn _mod_i(&self, b : &BigInt) -> BigUint
    {
        let one = 1.to_bigint().unwrap();
        b.modpow(&one, &self.modulo.to_bigint().expect("BigUint is valid BigInt")).to_biguint().expect("Result of modulo should be positive")
    }
}

impl Ring for ZZ_n<'_> {

    type Set = BigUint;

    fn add(&self, a : &Self::Set, b :  &Self::Set) -> Self::Set {
        self._mod(&(a + b))
    }

    fn add_inv(&self, a : &Self::Set) -> Self::Set {
        self.modulo - self._mod(&a)
    }

    fn mul(&self, a : &Self::Set, b :  &Self::Set) -> Self::Set {
        self._mod(&(a * b))
    }

}

impl Field for ZZ_n<'_> {

    fn mul_inv(&self, a : &Self::Set) -> Self::Set
    {
        let (g, s, t) = extended_euclid(&a.to_bigint().unwrap(), &self.modulo.to_bigint().unwrap());
        assert_eq!(g , 1.to_bigint().unwrap());
        println!("{:?} {:?} {:?}", g, s, t);
        self._mod_i(&s)
    }
}