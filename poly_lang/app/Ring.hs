{-# LANGUAGE DataKinds #-}

module Ring where

import Prelude hiding (negate)
import Data.Ratio
import Data.Poly.Multi.Semiring 
import Data.Euclidean
import Data.Semiring hiding ((+), (-), (*), (^), fromInteger)
import GHC.TypeNats

-- Saját Complex típus

data Complex a = !a :+ !a
    deriving (Eq)

infix 7 :+

instance (Show a, Num a, Eq a) => Show (Complex a) where
    show (a :+ b) = if (b == 0) then show a 
                                else show a ++ "+" ++ show b ++ "i"


conjugate :: (Num a) => Complex a -> Complex a
conjugate (a :+ b) = a :+ (- b)

{-
Összesen kell : 
    Num - (+), (*), (-)
    Fractional - (/)
    Integral - div, mod
    Num, Integral - (^)

    Vagyis minden Complex a -ra kell 
        Num Complex, 
        Fractional Complex, 
        Integral Complex
-}

(/+/) :: (Num a) => Complex a -> Complex a -> Complex a
(/+/) (a :+ b) (c :+ d) = (a + c) :+ (b + d)

(/*/) :: (Num a) => Complex a -> Complex a -> Complex a
(/*/) (a :+ b) (c :+ d) = (a * c - b * d) :+ (a * d + b * c)

(/-/) :: (Num a) => Complex a -> Complex a -> Complex a
(/-/) (a :+ b) (c :+ d) = (a - c) :+ (b - d)

absC :: (Num a) => Complex a -> Complex a
absC (a :+ b) = abs a :+ 0

signumC :: (Num a) => Complex a -> Complex a
signumC (a :+ b) = signum a :+ 0

fromIntegerC :: (Num a) => Integer -> Complex a
fromIntegerC a = fromInteger a :+ 0

instance Ring a => Semiring (Complex a) where
  zero = zero :+ zero
  one  = one  :+ zero
  plus  (x :+ y) (x' :+ y') = plus x x' :+ plus y y'
  times (x :+ y) (x' :+ y')
    = (x `times` x' `plus` (negate (y `times` y'))) :+ (x `times` y' `plus` y `times` x')
  fromNatural n = fromNatural n :+ zero


{-
instance Ring Rational where
    negate = (0-)
-}

instance (Num a) => Num (Complex a) where
    (+) = (/+/)
    (*) = (/*/)
    (-) = (/-/)
    abs = absC
    signum = signumC
    fromInteger = fromIntegerC

-- Wiki : https://en.wikipedia.org/wiki/Complex_number#Reciprocal_and_division
(///) :: (Fractional a, Num a) => Complex a -> Complex a -> Complex a
(///) z@(a :+ b) w@(c :+ d) = ((a * c + b * d) / denom ) :+ ((b * c - a * d) / denom)
    where denom = c ^ 2 + d ^ 2

instance (Fractional a, Num a) => Fractional (Complex a) where
    (/) = (///)
    fromRational r = (fromRational r :+ 0)

(.^) :: (Num a, Integral a) => Complex a -> a -> Complex a
(.^) z@(a :+ b) n | n <= 0    = 1 :+ 0
                  | otherwise = z * (z .^ (n - 1))

modCR :: Complex Rational -> Complex Rational -> Complex Rational
modCR z@(a :+ b) w@(c :+ d) = z - w * floorc (z / w)
    where
        floorc (x :+ y) = (round x % 1) :+ (round y % 1)

-- Integer division is always integer division
divC :: (Integral a) => Complex a -> Complex a -> Complex a
divC z@(a :+ b) w@(c :+ d) = (a `div` c) :+ 0

--- Rational

-- Num Rational

-- Fractional Rational

-- Integral Rational

instance Integral Rational where
    toInteger a = floor a
    quotRem a b = (quotR a b, remR a b)

-- Fractional mod a la https://functions.wolfram.com/IntegerFunctions/Mod/27/01/01/
-- We the take the whole park mod that then add the fractional part 
remR :: Rational -> Rational -> Rational
remR m n = m - n * (floor (m / n) % 1)

quotR :: Rational -> Rational -> Rational
quotR a b = (floor a `div` floor b) % 1

-- Also need Ord and Eq for all numbers, polinomials
--                                  Real
type Number = Rational
type N = Complex Rational

aC :: Complex Integer
aC = (1 :+ 2)

bC :: Complex Integer
bC = (1 :+ 3)

aR :: Rational
aR = (1 % 2)

bR :: Rational
bR = (1 % 3)

a' :: Complex Rational
a' =  (1 % 2) :+ (2 % 3)

b' :: Complex Rational
b' =  (2 % 3) :+ (1 % 3)

{-
    "+"
    "*"
    "-"
    "/"
    "div"
    "mod"
    "^"
-}

testRational :: [Rational -> Rational -> Rational]
testRational = 
    [ (+)
    , (-)
    , (*)
    , (/)
    , remR
    , quotR ]
--    (^)

testComplex :: [N -> N -> N]
testComplex =
    [ (/+/)
    , (/-/)
    , (/*/)
    , (///)
    , modCR ]
--   , div
--        (.^)

--- Polinomials

-- Need :
--      Field (Complex Rational)
--      Ring  (Complex Rational)
--      or more generally, for all 
--      Field/Ring a => Field/Ring (Complex a)

--p1 :: VMultiPoly 3 (Complex Rational)
--p1 = 2 * X + 3

p2 :: CPoly 1
p2 = 1 + X

type CPoly (n :: Nat) = VMultiPoly n (Complex Rational)

factor :: Number -> Number
factor a = undefined

irred :: Number -> Bool
irred a = undefined

derivative :: Number -> Number
derivative a = undefined

-- Num CPoly
binopPoly :: (KnownNat n) => [CPoly n -> CPoly n -> CPoly n]
binopPoly = 
    [ (+)
    , (-)
    , (*)
    ]

unaryPoly :: (KnownNat n) => [CPoly n -> CPoly n]
unaryPoly =
    [ abs
    , signum
    ]

-- Euclidian CPoly
{-
eucPoly :: [CPoly 1 -> CPoly 1 -> CPoly 1]
eucPoly = 
    [ Data.Euclidean.quot
    , Data.Euclidean.rem
    ]
-}