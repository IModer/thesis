{-# LANGUAGE DataKinds, TypeFamilies, UndecidableInstances #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Core.Types where

-- TODO
-- Cleanup/comments

import Lib

import Data.Maybe (fromJust, fromMaybe)
import Data.Ord   (comparing)
import Data.Tuple (swap)
import GHC.Natural (wordToNatural)

import Data.Semiring hiding (fromIntegral)
import Data.Euclidean
import Data.Ratio (numerator, denominator, (%))
import Prelude hiding ((*), (+), negate, (-), quot, rem, lcm, gcd)

-- Poly
import Data.Poly.Multi.Semiring
import qualified Data.Poly.Semiring as PS

-- Show Poly
import Data.List (intersperse, maximumBy, nub, uncons)
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Sized as SG
import qualified Data.Vector.Unboxed.Sized as SU
import qualified Data.Vector as V


import Data.Finite
import Data.Bifunctor (first, bimap)

-- Factor
import qualified Data.IntMap as IM
import Factor.Bz
import qualified Factor.Zx as Zx

-- Frac is a boxed Rational we can have custom Show

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
    (Box a) <= (Box b)  = x * y' <= x' * y
        where
            x = numerator a
            y = denominator a
            x' = numerator b
            y' = denominator b

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
    degree                  = error "frac degree"  -- we dont use degree
    quotRem x       y       = (quot x y,rem x y)
    quot    (Box x) (Box y) = floor (x / y) %% 1
    rem     (Box x) (Box y) = Box x - (Box y * floor (x / y) %% 1)

instance Field Frac
{- (/) is divide -}

-- Complex is a custom Data.Complex so we can have custom show 
-- and so we dont have RealFloat (https://hackage.haskell.org/package/complex-generic would do this but it isnt maintained) 

complexFracIsZ :: Complex Frac -> Bool
complexFracIsZ ((Box r) :+ c) = c == zero && b == one
    where
        b = denominator r

complexFracToZ :: Complex Frac -> Integer
complexFracToZ ((Box r) :+ _) = numerator r

data Complex a = !a :+ !a
    deriving (Eq)

real :: Complex a -> a
real (x :+ _) = x

imag :: Complex a -> a
imag (_ :+ y) = y

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
conjQuotAbs w@(x :+ y) = x' :+ negate y'
    where
        Just x' = x `divide` norm w
        Just y' = y `divide` norm w

norm :: Semiring a => Complex a -> a
norm (x :+ y) = (x `times` x) `plus` (y `times` y)

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

-- MultiPoly needs to know at compile time how long is it gonna be

-- Note: it could be solved by some magic (type level arithmetic with GADTS and type families)
-- but its beyond the scope of this project (also it would be easier in a dependently typed lang, like Agda, Idris)

type PolyMono = PS.VPoly

type Poly26 a = VMultiPoly 26 a

newtype PolyMulti a = BoxP (Poly26 a)
    deriving Eq

unPolyMulti :: PolyMulti a -> Poly26 a
unPolyMulti (BoxP p) = p

vars :: Integral a => [(a, Char)]
vars = zip [1..] ['A'..'Z']

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
                showVar k = maybe "" (:"") $ lookup (i+1) vars --(zip [1..] vars)
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
            (q',_) = toMonoPoly q
    rem p q  = fromMonoPoly (rem p' q') i
        where
            (p',i) = toMonoPoly p
            (q',_) = toMonoPoly q

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

-- We assume we only have 1 variable
instance Euclidean (PolyMulti Frac) where
    degree p = wordToNatural $ getPolyDegree p
    quot p q = fromMonoPoly (quot p' q') i
        where
            (p',i) = toMonoPoly p
            (q',_) = toMonoPoly q
    rem p q  = fromMonoPoly (rem p' q') i
        where
            (p',i) = toMonoPoly p
            (q',_) = toMonoPoly q

wzeros :: [Word]
wzeros = replicate 26 (0 :: Word)

unsafe :: Maybe a -> a
unsafe = fromJust

stringToComplexPoly :: String -> Maybe (PolyMulti (Complex Frac))
stringToComplexPoly s = do
    (hs, _) <- uncons s
    s' <- hs `lookup` map swap vars
    ls <- (SU.fromList $ replaceAtIndex (s' - 1) 1 wzeros :: Maybe (SU.Vector 26 Word))
    return $ BoxP (monomial ls one :: Poly26 (Complex Frac))

stringToPoly :: String -> Maybe (PolyMulti Frac)
stringToPoly s = do
    (hs, _) <- uncons s
    s' <- hs `lookup` map swap vars
    ls <- (SU.fromList $ replaceAtIndex (s' - 1) 1 wzeros :: Maybe (SU.Vector 26 Word))
    return $ BoxP (monomial ls one :: Poly26 Frac)

fracToComplexPoly :: Frac -> Maybe (PolyMulti (Complex Frac))
fracToComplexPoly f = do
    zeros <- SU.fromList wzeros :: Maybe (SU.Vector 26 Word)
    return $ BoxP $ toMultiPoly $ V.singleton (zeros , f :+ (0 %% 1))

fracToPoly :: Frac -> Maybe (PolyMulti Frac)
fracToPoly f = do
    zeros <- SU.fromList wzeros :: Maybe (SU.Vector 26 Word)
    return $ BoxP $ toMultiPoly $ V.singleton (zeros , f)

complexToComplexPoly :: Complex Frac -> PolyMulti (Complex Frac)
complexToComplexPoly cf = do
    let zeros = unsafe $ SU.fromList wzeros :: (SU.Vector 26 Word)
    BoxP $ toMultiPoly $ V.singleton (zeros , cf)

fracToComplex :: Frac -> Complex Frac
fracToComplex = (:+ (0 %% 1))

polyToCPoly :: PolyMulti Frac -> PolyMulti (Complex Frac)
polyToCPoly (BoxP p) = let a = unMultiPoly p in
                        let b = V.map (\(c,d) -> (c, d :+ (0 %% 1))) a in 
                            BoxP $ toMultiPoly b

getPolyDegree :: PolyMulti a -> Word
getPolyDegree (BoxP p) = let a = unMultiPoly p in
                            V.maximum $ V.map (SU.maximum . fst) a

getPolyNumOfVariables :: PolyMulti a -> Int
getPolyNumOfVariables (BoxP p) = let a = unMultiPoly p in
                                 let usa = V.map (V.convert . SU.fromSized . fst) a in
                                V.length $ 
                                V.filter id $ 
                                V.foldl1 (V.zipWith (||)) $ 
                                V.map (V.map (>0)) 
                                usa

loneVar :: (Eq a, Semiring a) => PolyMulti a -> Bool
loneVar (BoxP p) = let a = unMultiPoly p in
                    --V.map (SU.sum . fst) a
                    [(1,one)] == zip (V.toList (V.map (SU.sum . fst) a)) (V.toList $ V.map snd a)
                    --isJust (a V.!? 0) && V.length a == 1

-- This is like assembly programming, in that we look inside the implementation and guess the
-- vars from there
-- Note : This could be done with something called singletons, or again in a dependenty type language
-- How to read : first two lines then bottom up
whichVars :: PolyMulti a -> [Integer]
whichVars (BoxP p) = let a = unMultiPoly p in  --sized-vector repr. of p
                            let usa = V.map (V.convert . SU.fromSized . fst) a in -- convert that into a unsized vector (nicer to work with)
                            nub $                                 -- delete duplicates
                            V.toList $                            -- V.Vector a -> [a]
                            V.concat $                            -- [V.Vector a] -> V.Vector a =~ join (in vectors)
                            V.toList $                            -- [V.Vector Int]
                            V.filter (not . V.null) $             -- filter out []s (empty vectors)
                            V.map ( V.map fst .                   -- V.Vector (V.Vector Int) - only take the position
                                    V.filter ((>0) . snd) .       -- V.Vector (V.Vector (Int, Word))) - but only those that had its snd >0
                                    V.zip (V.fromList [0..26]))   -- V.Vector (V.Vector (Int, Word))) - zip with position
                            usa                                   -- usa : V.Vector (V.Vector Word) =~ [[Word]]

-- x is a lone variable of P, meaning P has x in it and x is just a variable with coeff 1
loneVarOf :: (Eq a, Semiring a) => PolyMulti a -> PolyMulti a -> Bool
loneVarOf x p = let vs = whichVars x in
                    loneVar x && (head vs `elem` whichVars p)

toMonoPoly :: (Eq a, Semiring a) => PolyMulti a -> (PolyMono a, Maybe Integer)
toMonoPoly (BoxP p) =  let a = unMultiPoly p in
                        let t = V.toList $ V.zip (V.map (fromIntegral . SU.sum . fst) a) (V.map snd a)  in
                        let m = fst $ maximumBy (comparing fst) t in
                        let c = [fromMaybe zero $ lookup i t | i <- [0..m] ] in --lookup ((fst i) :: Int) t
                        if null $ whichVars $ BoxP p 
                            then (PS.toPoly $ V.fromList c , Nothing)
                            else 
                                case whichVars $ BoxP p of
                                    []     -> error "argument to toMonoPoly, should be a single variable polinomial"
                                    [i] -> (PS.toPoly $ V.fromList c , Just i)
                                    _      -> error "argument to toMonoPoly, should be a single variable polinomial"
--                                let [i] = whichVars $ BoxP p in -- if there are multiple variables we fail here 

fromMonoPoly :: (Eq a, Semiring a) => PolyMono a -> Maybe Integer -> PolyMulti a
fromMonoPoly p (Just i) = let a = PS.unPoly p in
                        let ls j = unsafe (SU.fromList $ replaceAtIndex i (j :: Word) wzeros :: Maybe (SU.Vector 26 Word)) in
                        let b = V.map (\(x, n) -> (ls n,x)) $ V.zip a (V.fromList [(0 :: Word)..(fromIntegral $ V.length a)]) in
                        BoxP $ toMultiPoly b
fromMonoPoly p Nothing  = let a = PS.unPoly p in
                        let ls = unsafe (SU.fromList wzeros :: Maybe (SU.Vector 26 Word)) in
                        let b = V.map (\(x, _) -> (ls,x)) $ V.zip a (V.fromList [(0 :: Word)..(fromIntegral $ V.length a)]) in
                        BoxP $ toMultiPoly b

{-
-- Good for testing
testPoly :: PolyMulti Frac
testPoly =  let Just x = stringToPoly "X" in
            let Just y = stringToPoly "Y" in
            let Just z = stringToPoly "Z" in
            let Just four = fracToPoly (4 %% 1) in
            let Just tenthird = fracToPoly (10 %% 3) in
                --tenthird * x * x * x * z * z + four * {-y*-} x + x + four
                --x * x + tenthird * x + four + y
                x * x + four * four + z
                --x + four

testPoly2 :: PolyMulti Frac
testPoly2 =  let Just x = stringToPoly "X" in
            let Just y = stringToPoly "Y" in
            let Just z = stringToPoly "Z" in
            let Just four = fracToPoly (4 %% 1) in
            let Just tenthird = fracToPoly (10 %% 3) in
                --tenthird * x * x * x * z * z + four * {-y*-} x + x + four
                four * x * x * x * x * x + four * x * four * x + x + four

testCPoly :: PolyMulti (Complex Frac)
testCPoly = let Just x = stringToComplexPoly "X" in x

testCPoly2 :: PolyMulti (Complex Frac)
testCPoly2 = let Just x = stringToComplexPoly "X" in
             let Just y = stringToComplexPoly "Y" in
             let Just z = stringToComplexPoly "Z" in
             let Just four = complexToComplexPoly (4 %% 1 :+ zero) in
             let Just tenthird = complexToComplexPoly (10 %% 3 :+ zero) in
             --x * x + four * x * z + tenthird
             four * x * x * x * x * x + four * x * four * x + x + four

testComplex :: Complex Frac
testComplex = 4 %% 3 :+ zero
-}


-- subst [X, X* X , Z , X] (X + 2*X * Z + Z) = ((X * X) + 2 * (X * X) + X)
subst' :: [(PolyMulti (Complex Frac), PolyMulti (Complex Frac))] -> PolyMulti (Complex Frac) -> PolyMulti (Complex Frac)
subst' xs (BoxP p) = BoxP $ subst p (polys SG.// is xs)
    where
        is :: [(PolyMulti (Complex Frac), PolyMulti (Complex Frac))] -> [(Finite 26, Poly26 (Complex Frac))]
        is = map (bimap (finite . head . whichVars) unPolyMulti)
        
        -- polys, amik : [A,B,C,D,...,X,Y,Z]
        polys :: SG.Vector V.Vector 26 (Poly26 (Complex Frac))
        polys = unsafe $ 
                SG.fromList $ 
                map (unPolyMulti . unsafe . stringToComplexPoly . (: "")) $
                map snd $
                vars

-- eval [X, 1 , Z , 0] (X*X+2*X+Z) = 1*1+2*1+0
--         these should be loneVars of this
--              |                  |
--              V                  V
eval' :: (Semiring a, Eq a) => [(PolyMulti a, a)] -> PolyMulti a -> a
eval' xs (BoxP p) = eval p ((SG.//) ones (is xs))
    where
        is :: [(PolyMulti a, a)] -> [(Finite 26, a)]
        is = map (first (finite . head . whichVars))

        ones :: Semiring a => SG.Vector V.Vector 26 a
        ones = unsafe $ SG.fromList $ replicate 26 one

deriv' :: (Eq a, Semiring a) => Finite 26 -> PolyMulti a -> PolyMulti a
deriv' k (BoxP x) = BoxP $ deriv k x

derivativeByVar :: (Eq a, Semiring a) => PolyMulti a -> PolyMulti a -> PolyMulti a
derivativeByVar x p = let i = finite $ head $ whichVars x in deriv' i p

-- Factor

{-
-- Usefull for testing
testPolyZx :: Zx.Zx
testPolyZx = c 10 `m` ((x `m` x) `s` c 4) -- (x `s` c 3) `m` (x `s` c 3) `m` (x `s` c 3) --
    where
        x = Zx.variable
        m = Zx.multiply
        p = Zx.add
        s = (. Zx.negate) . Zx.add
        c = Zx.constant
-}

-- Decide if its in Zx, meaning all coeffs are in ZZ
polyIsZx :: PolyMulti (Complex Frac) -> Bool
polyIsZx (BoxP p) = let a = unMultiPoly p in
                let usa = a in
                V.and $ V.map (complexFracIsZ . snd) usa

-- If its a monoPoly and all coeffs are in ZZ then we convertt it
monoPolyToZx :: PolyMulti (Complex Frac) -> Maybe (Zx.Zx, Maybe Integer)
monoPolyToZx p =
    if getPolyNumOfVariables p == 1 && polyIsZx p
        then Just (p'', i) -- convert
        else Nothing
    where
        (p', i) = toMonoPoly p
        p'' = Zx.fromMonomials $
            zipWith Zx.Monomial ([0 .. ] :: [Int]) $
            map complexFracToZ $
            V.toList $
            PS.unPoly
            p'

zxToMultiPoly :: Zx.Zx -> Maybe Integer -> PolyMulti (Complex Frac)
zxToMultiPoly zx = fromMonoPoly p
    where
        p = foldr ((+) . (\(i,a) -> PS.monomial (fromIntegral i) (a %% 1 :+ zero))) zero $
            IM.toList $
            Zx.coeffMap
            zx

factor' :: Zx.Zx -> [Zx.Zx]
factor' x = map (Zx.multiply (Zx.constant a)) $
            concatMap (\(x',i) -> replicate (fromIntegral i) x')
            ls
    where
        (a , ls) = factor x

irred' :: Zx.Zx -> Bool
irred' = irreducible