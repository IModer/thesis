module Ring where

--import Data.Complex
--import Data.List

--TODO: move this ti some lib
zipWithPad :: (a -> b -> c) -> a -> b -> [a] -> [b] -> [c]
zipWithPad f da db xs []         = zipWith f xs (repeat db)
zipWithPad f da db [] ys         = zipWith f (repeat da) ys 
zipWithPad f da db (x:xs) (y:ys) = f x y : zipWithPad f da db xs ys

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

-- Also need Ord and Eq for all numbers, polinomials
--                                  Real
type Number = Integer

factor :: Number -> Number
factor a = undefined

irred :: Number -> Bool
irred a = undefined

derivative :: Number -> Number
derivative a = undefined

-- THIS IS ALL TODO
-- LIKE REALLY WORK ON IT LOSER

{-
data Number 
    = Integer Integer 
    | Rational Rational 
    | Double Double 
    | IntegerPoly (Poly Integer)
    | RationalPoly (Poly Rational)
    | DoublePoly (Poly Double)
    deriving Show
-}
    

class (Eq a) => Ring a where
    zero  :: a
    one   :: a

    add   :: a -> a -> a
    mul   :: a -> a -> a
    minus :: a -> a -> a
    neg   :: a -> a

    div   :: a -> a -> a
    mod   :: a -> a -> a
    pow   :: a -> a -> a

    minus a b = a `add` neg b

-- Integer
instance Ring Integer where
    zero = 0
    add  = (+)
    neg  = (-) 0
    one  = 1
    mul  = (*)

-- Integer functions here if needed

-- Rational
-- https://wiki.haskell.org/Rational

-- Ask : why'
{-
    All instance types must be of the form (T t1 ... tn)
    where T is not a synonym.
    Use TypeSynonymInstances if you want to disable this.

instance Ring Rational where
    zero = 0
    add  = (+)
    neg  = ((-) 0)
    one  = 1
    mul  = (*)
-}

-- Rational functions here if needed

-- Real
-- COMMENT: For now its a double but we should have a symbolic Real also

--instance (Real a) => Ring a where
--The constraint `Real a` is no smaller than the instance head `Ring a'   
instance Ring Double where
    zero = 0
    add  = (+)
    neg  = (-) 0
    one  = 1
    mul  = (*)

newtype Poly a = MkPoly [a] deriving (Show, Eq)


instance Functor Poly where
    fmap f (MkPoly a) = MkPoly $ fmap f a


polyPlus :: Ring a => Poly a -> Poly a -> Poly a
polyPlus (MkPoly f) (MkPoly g) = MkPoly $ zipWithPad add zero zero f g

polyNeg :: Ring a => Poly a -> Poly a
polyNeg (MkPoly f) = MkPoly $ map neg f

{-
BAD
poly_mul' :: Ring a => Poly a -> Poly a -> Poly a
poly_mul' (Poly xs) (Poly ys) = Poly (mul' xs ys)
    where
        mul' :: Ring a => [a] -> [a] -> [a]
        mul' []        _  = [zero]
        mul' (x : xs')  ys' = zipWith plus (map (mul x) ys') (zero : (mul' xs' ys'))
-}

-- TODO : this should be possible with Functor
polyShift :: Ring a => Poly a -> Poly a
polyShift (MkPoly f) = MkPoly $ zero:f

-- This function loses Poly
polyHead :: Ring a => Poly a -> a
polyHead (MkPoly f) = head f

-- TODO : Fix this is is bad
polyMul :: Ring a => Poly a -> Poly a -> Poly a
polyMul xs ys
    | xs == zero || ys == zero = zero
    | xs == one                = ys
    | ys == one                = xs
    | xs == MkPoly []            = MkPoly [zero] --hack     
    | otherwise                = polyPlus (fmap (mul $ polyHead xs) ys) (polyShift (polyMul xs ys) )

instance Ring a => Ring (Poly a) where
    zero = MkPoly [zero]
    add  = polyPlus
    neg  = polyNeg
    one  = MkPoly [one]
    mul  = polyMul

--- Tests

-- Integer : Constant 1
p1 :: Poly Integer
p1 = MkPoly [1]

-- Integer : X + 2
p2 :: Poly Integer
p2 = MkPoly [2, 1]

-- Integer : x^2 -x + 3
p3 :: Poly Integer
p3 = MkPoly [3,-1,2]

--Complex
--https://hackage.haskell.org/package/base-4.19.0.0/docs/Data-Complex.html

{- Ask
instance Ring (Complex a) where
    zero = 0
    add  = (+)
    neg  = ((-) 0)
    one  = 1
    mul  = (*)
-}

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