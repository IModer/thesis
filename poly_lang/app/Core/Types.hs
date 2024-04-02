{-# LANGUAGE DataKinds #-}

module Core.Types where

import Lib

import Data.Maybe (fromJust)

import Data.Semiring
import Data.Euclidean
import Data.Ratio (numerator, denominator, (%))
import Prelude hiding ((*), (+), negate, (-), quot, rem, lcm, gcd)

-- Poly
import Data.Poly.Multi.Semiring

-- Show Poly
import Data.List (intersperse)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector as V

import Data.Finite

-- Frac is a boxed Rational we can have custom Show

--type Frac = WrappedFractional Rational

{--}
newtype Frac = Box Rational
    deriving (Eq)

(%%) :: Integer -> Integer -> Frac
a %% b = Box $ a % b

instance Show Frac where
    show (Box r) = if b == 1
                    then show a
                    else show a ++ "/" ++ show b
        where
            a = numerator r
            b = denominator r

--https://hackage.haskell.org/package/base-4.19.1.0/docs/src/GHC.Real.html#line-550
instance Ord Frac where
--    compare (Box a) <= (Box b) = 
    (Box a) <= (Box b)  = x * y' <= x' * y
        where
            x = numerator a
            y = denominator a
            x' = numerator b
            y' = denominator b

-- This might be cleaner with something like a monofunctor
instance Semiring Frac where
    plus (Box a) (Box b) = Box (a + b)
    times (Box a) (Box b) = Box (a * b)
    zero = Box 0
    one = Box 1

instance Ring Frac where
    negate (Box a) = Box $ negate a

---https://hackage.haskell.org/package/semirings-0.6/docs/src/Data.Euclidean.html#line-274
instance GcdDomain Frac where
    divide (Box x) (Box y) = Just $ Box $ x / y
    gcd                    = const $ const $ Box 1
    lcm                    = const $ const $ Box 1
    coprime                = const $ const True

instance Euclidean Frac where
    degree                  = const 0
    quotRem (Box x) (Box y) = (Box (x / y), Box 0)
    quot    (Box x) (Box y) = Box (x / y)
    rem                     = const $ const $ Box 0

instance Field Frac
{--}

-- Complex is a custom Data.Complex so we can have custom show 
-- and so we dont have RealFloat (https://hackage.haskell.org/package/complex-generic would do this but it isnt maintained) 

data Complex a = !a :+ !a 
    deriving (Eq)

infix 6 :+

instance (Show a, Eq a, Semiring a) => Show (Complex a) where
    show (a :+ b) = if b == zero then show a 
                                 else show a ++ "+" ++ show b ++ "i"

--https://hackage.haskell.org/package/semirings-0.6/docs/src/Data.Semiring.html#line-633
instance Ring a => Semiring (Complex a) where
    zero = zero :+ zero
    one  = one  :+ zero
    plus  (x :+ y) (x' :+ y') = plus x x' :+ plus y y'
    times (x :+ y) (x' :+ y') = (x * x' - (y * y')) :+ (x * y' + y * x')
    fromNatural n = fromNatural n :+ zero

instance Ring a => Ring (Complex a) where
  negate (x :+ y) = negate x :+ negate y

-- https://hackage.haskell.org/package/semirings-0.6/docs/src/Data.Euclidean.html#line-349

conjQuotAbs :: Field a => Complex a -> Complex a
conjQuotAbs (x :+ y) = x `quot` norm :+ negate y `quot` norm
    where
        norm = (x `times` x) `plus` (y `times` y)

instance Field a => GcdDomain (Complex a) where
    divide x y = Just (x `times` conjQuotAbs y)
    gcd        = const $ const one
    lcm        = const $ const one
    coprime    = const $ const True

instance Field a => Euclidean (Complex a) where
    degree      = const 0
    quotRem x y = (quot x y, zero)
    quot x y    = x `times` conjQuotAbs y
    rem         = const $ const zero

instance Field a => Field (Complex a)

-- Multivariate polynomials, boxed so we can have nicer show

--  (Eq a, Semiring a, KnownNat n, Vector v (Vector n Word, a)) => Semiring (MultiPoly v n a)
--  (Eq a, Ring a, KnownNat n, Vector v (Vector n Word, a)) => Ring (MultiPoly v n a)
--  (Eq a, Ring a, GcdDomain a, KnownNat n, forall (m :: Nat). KnownNat m => Vector v (Vector m Word, a), forall (m :: Nat). KnownNat m => Eq (v (Vector m Word, a))) => GcdDomain (MultiPoly v n a)
--  (Eq a, Field a, Vector v (Vector 1 Word, a)) => Euclidean (Poly v a)

-- MultiPoly needs to know at compile time how long is it gonna be
-- Edit: it could be solved by some magic but its beyond the scope of this project

type Poly26 a = VMultiPoly 26 a

newtype PolyMulti a = BoxP (Poly26 a)
    deriving Eq

vars :: [Char]
vars = ['A'..'Z']

{-
-}
instance Show a => Show (PolyMulti a) where
    -- https://hackage.haskell.org/package/poly-0.5.1.0/docs/src/Data.Poly.Internal.Multi.html#MultiPoly
    showsPrec d (BoxP p)
        | G.null xs = showString "0"
        | otherwise = showParen (d > 0)
                        $ foldl (.) id
                        $ intersperse (showString " + ")
                        $ G.foldl (\acc (is, c) -> showCoeff is c : acc) [] xs
            where
                xs = unMultiPoly p

                showCoeff is c = showsPrec 7 c . foldl (.) id
                                ( map ((showString " * " .) . uncurry showPower)
                                $ filter ((/= 0) . fst)
                                $ zip (SU.toList is) (finites :: [Finite 26]))
                
                showPower :: Word -> Finite n -> String -> String
                showPower 1 n = showString (showVar n)
                showPower i n = showString (showVar n) . showString ("^" ++ show i)
                -- We Show Vars differently
                showVar :: Finite n -> String
                showVar k = maybe "" (:"") $ lookup i (zip [1..] vars)
                    where
                        i = getFinite k

-- Complex Poly

instance Semiring (PolyMulti (Complex Frac)) where
    zero = unsafe $ fracToComplexPoly (0 %% 1)
    one  = unsafe $ fracToComplexPoly (1 %% 1)
    plus  (BoxP a) (BoxP b) = BoxP $ a + b
    times (BoxP a) (BoxP b) = BoxP $ a * b
    fromNatural n = unsafe $ fracToComplexPoly (fromNatural n %% 1)

instance Ring (PolyMulti (Complex Frac)) where
    negate (BoxP p) = BoxP $ negate p

instance GcdDomain (PolyMulti (Complex Frac)) where
    divide  (BoxP x) (BoxP y) = Just $ BoxP $ unsafe $ x `divide` y
    gcd     (BoxP x) (BoxP y) = BoxP $ x `gcd` y
    lcm     (BoxP x) (BoxP y) = BoxP $ x `lcm` y
    coprime (BoxP x) (BoxP y) = x `coprime` y

instance Euclidean (Poly26 (Complex Frac)) where
    degree = degree 
    quot = quot
    rem  = rem

instance Euclidean (PolyMulti (Complex Frac)) where
    degree (BoxP x)          = degree x
    quot   (BoxP x) (BoxP y) = BoxP (x `quot` y)
    rem    (BoxP x) (BoxP y) = BoxP (x `rem` y)

instance Semiring (PolyMulti Frac) where
    zero = unsafe $ fracToPoly (0 %% 1)
    one  = unsafe $ fracToPoly (1 %% 1)
    plus  (BoxP a) (BoxP b) = BoxP $ a + b
    times (BoxP a) (BoxP b) = BoxP $ a * b
    fromNatural n = unsafe $ fracToPoly (fromNatural n %% 1)

instance Ring (PolyMulti Frac) where
    negate (BoxP p) = BoxP $ negate p

instance GcdDomain (PolyMulti Frac) where
    divide  (BoxP x) (BoxP y) = Just $ BoxP $ unsafe $ x `divide` y
    gcd     (BoxP x) (BoxP y) = BoxP $ x `gcd` y
    lcm     (BoxP x) (BoxP y) = BoxP $ x `lcm` y
    coprime (BoxP x) (BoxP y) = x `coprime` y

instance Euclidean (Poly26 Frac) where
    degree = degree 
    quot = quot
    rem  = rem

instance Euclidean (PolyMulti Frac) where
    degree (BoxP x)          = degree x
    quot   (BoxP x) (BoxP y) = BoxP (x `quot` y)
    rem    (BoxP x) (BoxP y) = BoxP (x `rem` y)

wzeros :: [Word]
wzeros = replicate 26 (0 :: Word)

unsafe = fromJust

stringToComplexPoly :: String -> Maybe (PolyMulti (Complex Frac))
stringToComplexPoly s =
    if length s /= 1
        then Nothing -- not a single letter like `var XY`
        else do
            s' <- lookup (head s) (zip vars [1..])
            ls <- (SU.fromList $ replaceAtIndex s' (1 :: Word) wzeros :: Maybe (SU.Vector 26 Word))
            return $ BoxP (monomial ls ((1 %% 1) :+ (0 %% 1)) :: Poly26 (Complex Frac))

stringToPoly :: String -> Maybe (PolyMulti Frac)
stringToPoly s = 
    if length s /= 1
        then Nothing -- not a single letter like `var XY`
        else do
            s' <- lookup (head s) (zip vars [1..])
            ls <- (SU.fromList $ replaceAtIndex s' (1 :: Word) wzeros :: Maybe (SU.Vector 26 Word))
            return $ BoxP (monomial ls (1 %% 1) :: Poly26 Frac)

fracToComplexPoly :: Frac -> Maybe (PolyMulti (Complex Frac))
fracToComplexPoly f = do
    zeros <- SU.fromList wzeros :: Maybe (SU.Vector 26 Word)
    return $ BoxP $ toMultiPoly $ V.singleton (zeros , f :+ (0 %% 1))

fracToPoly :: Frac -> Maybe (PolyMulti Frac)
fracToPoly f = do
    zeros <- SU.fromList wzeros :: Maybe (SU.Vector 26 Word)
    return $ BoxP $ toMultiPoly $ V.singleton (zeros , f)

complexToComplexPoly :: Complex Frac -> Maybe (PolyMulti (Complex Frac))
complexToComplexPoly cf = do
    zeros <- SU.fromList wzeros :: Maybe (SU.Vector 26 Word)
    return $ BoxP $ toMultiPoly $ V.singleton (zeros , cf)

-- This cannot be done, but isnt needed as we always cast up
{-
complexToPoly :: Complex Frac -> Maybe (PolyMulti Frac)
-}

-- Extra polinomial functions on PolyMulti

irred = undefined

factor = undefined

derivative = undefined