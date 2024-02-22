pub mod core;

fn main() {
    
}

mod tests {

    #[test]
    fn zz_n_arithmetic() {
        use crate::core::numbers::{ZZ_n, Ring, Field};
        use num_bigint::ToBigUint;

        let three = 3.to_biguint().unwrap();

        let zz_3 = ZZ_n::new(&three);

        let zero = 0.to_biguint().unwrap();
        let one  = 1.to_biguint().unwrap();
        let two  = 2.to_biguint().unwrap();

        //Addition
        assert_eq!(zz_3.add(&one, &two), zero);

        //Additive Inverse
        assert_eq!(zz_3.add_inv(&one), two);
        assert_eq!(zz_3.add_inv(&two), one);

        //Big numbers
        let lot1  = 2147483647.to_biguint().unwrap();
        let lot2  = 2147483647.to_biguint().unwrap();
        let lots = lot1 * lot2;

        //Addition with additive inverse
        assert_eq!(zz_3.add( &zz_3.add_inv(&lots), &lots), zero);

        //Multiplicative inverse
        assert_eq!(zz_3.mul_inv(&two), two);
        assert_eq!(zz_3.mul_inv(&one), one);
    }


}