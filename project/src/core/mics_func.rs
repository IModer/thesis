use num_bigint::{BigInt, ToBigInt};

pub fn extended_euclid(a : &BigInt, b: &BigInt) -> (BigInt, BigInt, BigInt)
{
    let zero = 0.to_bigint().unwrap();
    let one  = 1.to_bigint().unwrap(); 

    let mut c = a.clone();
    let mut d = b.clone();
    
    let mut c_1 = one.clone();
    let mut c_2 = zero.clone();
    let mut d_1 = zero.clone();
    let mut d_2 = one;
    
    while d != zero {
        let q = c.clone() / d.clone(); let r = c - q.clone() * d.clone();
        let r_1 = c_1 - q.clone() * d_1.clone(); let r_2 = c_2 - q * d_2.clone();
        c = d; c_1 = d_1; c_2 = d_2;
        d = r; d_1 = r_1; d_2 = r_2;
    }

    (c, c_1, c_2)

}