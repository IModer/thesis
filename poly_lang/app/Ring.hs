{-# LANGUAGE DataKinds #-}

module Ring where

import Prelude hiding (negate, replicate, init)
import Data.Ratio
-- Poly
import qualified Data.Poly.Semiring as PS
import Data.Poly.Multi.Semiring

-- Show
import Data.List (intersperse)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Sized as SU

import Data.Maybe (fromJust) 

import Data.Euclidean
import Data.Semiring hiding ((+), (-), (*), (^), fromInteger)
import GHC.TypeNats
import Data.Finite
import Lib
import GHC.Num

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
    = (x `times` x' `plus` (Data.Semiring.negate (y `times` y'))) :+ (x `times` y' `plus` y `times` x')
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

newtype Rat = Rat Rational
    deriving (Eq)

instance Show Rat where
    show (Rat r) = if b == 1
                    then show a
                    else show a ++ "/" ++ show b
        where
            a = numerator r
            b = denominator r

instance Num Rat where
    (Rat ap) + (Rat bp) = Rat $ ap + bp
    (Rat ap) - (Rat bp) = Rat $ ap + bp
    (Rat ap) * (Rat bp) = Rat $ ap + bp

    abs    (Rat p) = Rat $ abs p
    signum (Rat p) = Rat $ signum p
    negate (Rat p) = Rat $ GHC.Num.negate p
    fromInteger i = Rat $ fromInteger i

instance Integral Rational where
    toInteger a = floor a
    quotRem a b = (quotR a b, remR a b)

-- Fractional mod a la https://functions.wolfram.com/IntegerFunctions/Mod/27/01/01/
-- We the take the whole part mod that then add the fractional part
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

--p2 :: CPoly 1
--p2 = 1 + X

type CPolyMono = PS.VPoly (Complex Rational)

-- ? type CPolyMulti = forall n. VMultiPoly n (Complex Rational) -- TODO : good question if this can be made variable

type CPoly (n :: Nat) = VMultiPoly n (Complex Rational)

newtype CPolyMulti = Box (VMultiPoly 26 (Complex Rational))

vars = ['A'..'Z']

instance Show CPolyMulti where
    -- This is from : https://hackage.haskell.org/package/poly-0.5.1.0/docs/src/Data.Poly.Internal.Multi.html#MultiPoly
    showsPrec d (Box p)
        | G.null (unMultiPoly p) = showString "0"
        | otherwise = showParen (d > 0)
                        $ foldl (.) id
                        $ intersperse (showString " + ")
                        $ G.foldl (\acc (is, c) -> showCoeff is c : acc) [] (unMultiPoly p)
            where
                showCoeff is c = showsPrec 7 c . foldl (.) id
                                ( map ((showString " * " .) . uncurry showPower)
                                $ filter ((/= 0) . fst)
                                $ zip (SU.toList is) (finites :: [Finite 26]))
                
                showPower :: Word -> Finite n -> String -> String
                showPower 1 n = showString (showVar n)
                showPower i n = showString (showVar n) . showString ("^" ++ show i)
                -- but we show Vars differently
                showVar :: Finite n -> String
                showVar k = maybe "" (:"") $ lookup i (zip [1..] vars)
                    where
                        i = getFinite k

toPolyMulti :: String -> Maybe CPolyMulti
toPolyMulti s =
    if (length s) /= 1 
        then Nothing
        else do
            s' <- lookup (s !! 0) (zip vars [1..])
            ls <- (SU.fromList $ replaceAtIndex s' (1 :: Word) (take 26 $ repeat (0 :: Word)) :: Maybe (SU.Vector 26 Word))
            return $ Box $ (monomial ls 1 :: VMultiPoly 26 (Complex Rational))
    where

instance Num CPolyMulti where
    (Box ap) + (Box bp) = Box $ ap + bp
    (Box ap) - (Box bp) = Box $ ap + bp
    (Box ap) * (Box bp) = Box $ ap + bp

    abs (Box p) = Box $ abs p
    signum (Box p) = Box $ signum p
    negate (Box p) = Box $ GHC.Num.negate p
    fromInteger i = Box $ fromInteger i


--toPolyMulti n = monomial (oneAtN n) 1
--    where
--        oneAtN :: Nat -> Vector n a
--        oneAtN n = freeze $ write (thaw $ replicate 0) (finite $ toInteger n) 1

toPolyMono :: String -> CPolyMono
toPolyMono s = PS.monomial 1 1

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