module Ring where

--import Data.Complex
--import Data.List
-- TODO : Look into complex-generic
import Prelude hiding (mod, div)
--import Data.Complex
import Data.Ratio

--TODO: move this to some lib
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

-- Saját Complex típus

data Complex a = !a :+ !a
    deriving (Eq, Show)

infix 7 :+

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

(.^) :: (Num a, Integral a) => Complex a -> a -> Complex a
(.^) z@(a :+ b) n | n <= 0    = 1 :+ 0
                  | otherwise = z * (z .^ (n - 1))

mod :: Complex a -> Complex a -> Complex a
mod = undefined

div :: Complex a -> Complex a -> Complex a
div = undefined

-- Also need Ord and Eq for all numbers, polinomials
--                                  Real
type Number = Integer
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

test :: [N -> N -> N]
test = [(/+/), 
        (/*/), 
        (/-/),
        (///), 
        div, 
        mod
--        (.^)
        ]

factor :: Number -> Number
factor a = undefined

irred :: Number -> Bool
irred a = undefined

derivative :: Number -> Number
derivative a = undefined