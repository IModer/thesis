{-# LANGUAGE DataKinds #-}

module Core.Types where

import Lib

import Control.Exception

import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Ord   (comparing)
import GHC.Natural (wordToNatural)

import Data.Semiring hiding (fromIntegral)
import Data.Euclidean
import Data.Ratio (numerator, denominator, (%))
import Prelude hiding ((*), (+), negate, (-), quot, rem, lcm, gcd)

-- Poly
import Data.Poly.Multi.Semiring
import qualified Data.Poly.Semiring as PS
--import Data.Poly.Internal.Multi.Field

-- Show Poly
import Data.List (intersperse, maximumBy, nub)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU

import Data.Finite

-- Frac is a boxed Rational we can have custom Show

--type Frac = WrappedFractional Rational

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
    degree                  = error "frac degree"  -- this isnt used anywhere
    quotRem x       y       = (quot x y,rem x y)
    quot    (Box x) (Box y) = floor (x / y) %% 1
    rem     (Box x) (Box y) = Box x - (Box y * floor (x / y) %% 1)

{-
instance Euclidean Frac where
    degree                  = const 0
    quotRem (Box x) (Box y) = (Box (x / y), Box 0)
    quot    (Box x) (Box y) = Box (x / y)
    rem                     = const $ const $ Box 0
-}

instance Field Frac
{--}

-- Complex is a custom Data.Complex so we can have custom show 
-- and so we dont have RealFloat (https://hackage.haskell.org/package/complex-generic would do this but it isnt maintained) 

data Complex a = !a :+ !a
    deriving (Eq)

real :: Complex a -> a
real (x :+ y) = x

imag :: Complex a -> a
imag (x :+ y) = y

instance (Ord a, Semiring a, Eq a) => Ord (Complex a) where
    compare (x :+ y) (x' :+ y') = if y * y' == zero 
                                    then compare x x'
                                    else EQ

infix 6 :+

instance (Show a, Eq a, Semiring a) => Show (Complex a) where
    show (a :+ b) = case (a == zero, b == zero) of
                        (True  , True ) -> "0"
                        (False , True ) -> show a
                        (True  , False) -> show b ++ "i"
                        (False , False) -> show a ++ "+" ++ show b ++ "i"

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

instance (Field a, Eq a) => GcdDomain (Complex a) where
    divide z@(x :+ y) w@(x' :+ y') = if y == zero && y' == zero
                                        then do {k <- x `divide` x';
                                            return $ k :+ zero}
                                        else Just (z `times` conjQuotAbs w)
    gcd        = const $ const one
    lcm        = const $ const one
    coprime    = const $ const True

instance (Field a, Eq a) => Euclidean (Complex a) where
    degree      = const 0
    quotRem x y = (quot x y, rem x y)
    quot z@(x :+ y) w@(x' :+ y') = if y == zero && y' == zero
                                    then x `quot` x' :+ zero
                                    else z `times` conjQuotAbs w
    rem  (x :+ y) (x' :+ y') = if y == zero && y' == zero
                                then x `rem` x' :+ zero
                                else zero

instance (Field a, Eq a) => Field (Complex a)

-- Multivariate polynomials, boxed so we can have nicer show

--  (Eq a, Semiring a, KnownNat n, Vector v (Vector n Word, a)) => Semiring (MultiPoly v n a)
--  (Eq a, Ring a, KnownNat n, Vector v (Vector n Word, a)) => Ring (MultiPoly v n a)
--  (Eq a, Ring a, GcdDomain a, KnownNat n, forall (m :: Nat). KnownNat m => Vector v (Vector m Word, a), forall (m :: Nat). KnownNat m => Eq (v (Vector m Word, a))) => GcdDomain (MultiPoly v n a)
--  (Eq a, Field a, Vector v (Vector 1 Word, a)) => Euclidean (Poly v a)

-- MultiPoly needs to know at compile time how long is it gonna be
-- Edit: it could be solved by some magic but its beyond the scope of this project

type PolyMono = PS.VPoly

type Poly26 a = VMultiPoly 26 a

newtype PolyMulti a = BoxP (Poly26 a)
    deriving Eq

unPolyMulti :: PolyMulti a -> Poly26 a
unPolyMulti (BoxP p) = p

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
                showVar k = maybe "" (:"") $ lookup (i+1) (zip [1..] vars)
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

instance Euclidean (PolyMulti (Complex Frac)) where
    degree p = wordToNatural $ getPolyDegree p
    quot p q = fromMonoPoly (quot p' q') i
        where
            (p',i) = toMonoPoly p
            (q',j) = toMonoPoly q
    rem p q  = fromMonoPoly (rem p' q') i
        where
            (p',i) = toMonoPoly p
            (q',j) = toMonoPoly q

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

-- This only works if we only have 1 variable
instance Euclidean (PolyMulti Frac) where
    degree p = wordToNatural $ getPolyDegree p
    quot p q = fromMonoPoly (quot p' q') i
        where
            (p',i) = toMonoPoly p
            (q',j) = toMonoPoly q
    rem p q  = fromMonoPoly (rem p' q') i
        where
            (p',i) = toMonoPoly p
            (q',j) = toMonoPoly q

wzeros :: [Word]
wzeros = replicate 26 (0 :: Word)

unsafe = fromJust

stringToComplexPoly :: String -> Maybe (PolyMulti (Complex Frac))
stringToComplexPoly s =
    if length s /= 1
        then Nothing -- not a single letter like `var XY`
        else do
            s' <- lookup (head s) (zip vars [1..])
            ls <- (SU.fromList $ replaceAtIndex (s'-1) (1 :: Word) wzeros :: Maybe (SU.Vector 26 Word))
            return $ BoxP (monomial ls ((1 %% 1) :+ (0 %% 1)) :: Poly26 (Complex Frac))

stringToPoly :: String -> Maybe (PolyMulti Frac)
stringToPoly s = 
    if length s /= 1
        then Nothing -- not a single letter like `var XY`
        else do
            s' <- lookup (head s) (zip vars [1..])
            ls <- (SU.fromList $ replaceAtIndex (s'-1) (1 :: Word) wzeros :: Maybe (SU.Vector 26 Word))
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

fracToComplex :: Frac -> Complex Frac
fracToComplex = (:+ (0 %% 1))

polyToCPoly :: PolyMulti Frac -> PolyMulti (Complex Frac)
polyToCPoly (BoxP p) = let a = unMultiPoly p in
                        let b = V.map (\(a,b) -> (a, b :+ (0 %% 1))) a in 
                            BoxP $ toMultiPoly b

getPolyDegree :: PolyMulti a -> Word
getPolyDegree (BoxP p) = let a = unMultiPoly p in
                            V.maximum $ V.map (SU.maximum . fst) a

getPolyNumOfVariables :: PolyMulti a -> Int
getPolyNumOfVariables (BoxP p) = let a = unMultiPoly p in
                                    let usa = V.map (V.convert . SU.fromSized . fst) a in
                                        V.length $ V.filter id $ V.foldl1 (V.zipWith (||)) $ V.map (V.map (>0)) usa
{-
ifMonoWhichVar :: PolyMulti a -> Int
ifMonoWhichVar (BoxP p) = let a = unMultiPoly p in
                            let usa = V.map (V.convert . SU.fromSized . fst) a in
                            V.maximum $  V.map (fromMaybe 0 . V.elemIndex True) $ V.map (V.map (>0)) usa
-} 

--numOfCoeffs :: PolyMulti a -> V.Vector (V.Vector Word, a)
numOfCoeffs (BoxP p) = let a = unMultiPoly p in
                let usa =  a in
                usa

loneVar :: (Eq a, Semiring a) => PolyMulti a -> Bool
loneVar (BoxP p) = let a = unMultiPoly p in
                    --V.map (SU.sum . fst) a
                    [(1,one)] == zip (V.toList (V.map (SU.sum . fst) a)) (V.toList $ V.map snd a)
                    --isJust (a V.!? 0) && V.length a == 1

whichVars :: PolyMulti a -> [Integer]
whichVars (BoxP p) = let a = unMultiPoly p in
                            let usa = V.map (V.convert . SU.fromSized . fst) a in
                            nub $ V.toList $ V.concat $ V.toList $ V.filter (V.fromList [] /=) $ V.map (V.map fst . V.filter ((>0) . snd) . V.zip (V.fromList [0..26])) usa
                            --V.map (fromMaybe 0 . V.elemIndex True) $ V.map (V.map (>0)) usa

-- x is a lose variable of P, meaning P has x in it and x is just a variable with coeff 1
loneVarOf :: (Eq a, Semiring a) => PolyMulti a -> PolyMulti a -> Bool
loneVarOf x p = let vs = whichVars x in
                    loneVar x && (head vs `elem` whichVars p)

toMonoPoly :: (Eq a, Semiring a) => PolyMulti a -> (PolyMono a, Integer)
toMonoPoly (BoxP p) =  let a = unMultiPoly p in
                        let t = V.toList $ V.zip (V.map (fromIntegral . SU.sum . fst) a) (V.map snd a)  in
                        let m = fst $ maximumBy (comparing fst) t in
                        let c = [fromMaybe zero $ lookup i t | i <- [0..m] ] in --lookup ((fst i) :: Int) t
                        let [i] = whichVars $ BoxP p in -- if there are multiple variables we fail here 
                        (PS.toPoly $ V.fromList c , i)

fromMonoPoly :: (Eq a, Semiring a) => PolyMono a -> Integer -> PolyMulti a
fromMonoPoly p i = let a = PS.unPoly p in
                        let ls j = unsafe (SU.fromList $ replaceAtIndex i (j :: Word) wzeros :: Maybe (SU.Vector 26 Word)) in
                        let b = V.map (\(x, n) -> (ls n,x)) $ V.zip a (V.fromList [0..(fromIntegral $ V.length a)]) in
                        BoxP $ toMultiPoly b

testPoly :: PolyMulti Frac
testPoly =  let Just x = stringToPoly "X" in
            let Just y = stringToPoly "Y" in
            let Just z = stringToPoly "Z" in
            let Just four = fracToPoly (4 %% 1) in
            let Just tenthird = fracToPoly (10 %% 3) in
                --tenthird * x * x * x * z * z + four * {-y*-} x + x + four
                --x * x + tenthird * x + four + y
                x * x * z + four + z

testPoly2 :: PolyMulti Frac
testPoly2 =  let Just x = stringToPoly "X" in
            let Just y = stringToPoly "Y" in
            let Just z = stringToPoly "Z" in
            let Just four = fracToPoly (4 %% 1) in
            let Just tenthird = fracToPoly (10 %% 3) in
                --tenthird * x * x * x * z * z + four * {-y*-} x + x + four
                four * x * x * x * x * x + four * x * four * x + x + four

newtype EuclidException = EucExc String
    deriving (Show, Eq)

instance Exception EuclidException where
    displayException (EucExc s) = s

test :: IO()
test = do 
    e <- (try (evaluate (testPoly `rem` testPoly2)) :: IO(Either EuclidException (PolyMulti Frac)))
    case e of
        Left a -> print a
        Right a -> print a

-- This cannot be done, but isnt needed as we always cast up
{-
complexToPoly :: Complex Frac -> Maybe (PolyMulti Frac)
-}

-- Extra polinomial functions on PolyMulti

irred = undefined

factor = undefined


deriv' :: (Eq a, Semiring a) => Finite 26 -> PolyMulti a -> PolyMulti a
deriv' k (BoxP x) = BoxP $ deriv k x

derivativeVar :: (Eq a, Semiring a) => PolyMulti a -> PolyMulti a -> PolyMulti a
derivativeVar x p = let i = finite $ head $ whichVars x in deriv' i p

derivative :: (Eq a, Semiring a) => Complex Frac -> PolyMulti a -> PolyMulti a
derivative k x = deriv' (finite k') x
    where 
        ((Box a) :+ _) = k
        k' = numerator a

{-
derivative :: (Eq a, Semiring a) => PolyMulti a -> PolyMulti a
derivative x= go (whichVars x) x
    where
        --go :: Finite 26 -> PolyMulti (Complex Frac) -> PolyMulti (Complex Frac)
        go [] x' = x'
        go (i:is) x' = go is (deriv' (finite i) x')
-}