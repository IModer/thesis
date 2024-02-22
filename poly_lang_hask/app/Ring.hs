module Ring where

import Data.Complex

{- |  
Ring a
Its main use is in the Polynomial typeclass
It should satisfy the following laws
@
    (+):
    a + (b + c) == (a + b) + c
    a + zero == zero + a == a
    a + (-a) == (-a) + a == zero

    (*):
    a * (b * c) == (a * b) * c
    a * one == one + a == a
@
-}

class Ring a where
    zero  :: a
    plus  :: a -> a -> a
    neg   :: a -> a
    one   :: a
    mul   :: a -> a -> a

-- Integer
instance Ring Integer where
    zero = 0
    plus = (+)
    neg  = ((-) 0)
    one  = 1
    mul  = (*)

-- Integer functions here if needed

-- Rational
-- https://wiki.haskell.org/Rational

instance Ring Rational where
    zero = 0
    plus = (+)
    neg  = ((-) 0)
    one  = 1
    mul  = (*)

-- Rational functions here if needed

-- Real
-- COMMENT: For now its a double but we should have a symbolic Real also

--instance (Real a) => Ring a where
--The constraint `Real a` is no smaller than the instance head `Ring a'   
instance Ring Double where
    zero = 0
    plus = (+)
    neg  = ((-) 0)
    one  = 1
    mul  = (*)

--Complex
--https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Complex.html

instance Ring (Complex Double) where
    zero = 0
    plus = (+)
    neg  = ((-) 0)
    one  = 1
    mul  = (*)

{- 
Nat

data Nat  = 
    Zero
    | Suc Nat

plusNat Zero y = y
plusNat (Suc x) y = Suc (plusNat x y)

timesNat Zero y = Zero
timesNat (Suc x) y = plusNat y $ timesNat x y

instance Number Nat where
    (+) = plusNat
    (*) = timesNat
-}