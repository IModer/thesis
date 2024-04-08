{-# LANGUAGE PatternSynonyms #-}

module Lib where

import Control.Applicative

-- Error type for typechecker
{-
type Error = Either String

throwError :: String -> Either String a
throwError = Left

pattern Error :: String -> Error a
pattern Error s = Left s

pattern Result :: a -> Error a
pattern Result a = Right a
-}

-- 3Option for Parser

data Option a b c = OLeft a | OMiddle b | ORight c
    deriving (Show, Eq)

option :: (a -> d) -> (b -> d) -> (c -> d) -> Option a b c -> d
option f g h o = case o of
    OLeft a   -> f a
    OMiddle b -> g b
    ORight c  -> h c

isOLeft :: Option a b c -> Bool
isOLeft = option (const True) (const False) (const False)

isOMiddle :: Option a b c -> Bool
isOMiddle = option (const False) (const True) (const False)

isORight :: Option a b c -> Bool
isORight = option (const False) (const False) (const True)

partitionOptions :: [Option a b c] -> ([a], [b], [c])
partitionOptions = foldr (option left middle right) ([], [] , [])
    where
        left   a ~(l, m, r) = (a:l, m   , r  )
        middle a ~(l, m, r) = (l  , a:m , r  )
        right  a ~(l, m, r) = (l  , m   , a:r)

fst3 :: (a,b,c) -> a
fst3 (a,_,_) = a

snd3 :: (a,b,c) -> b
snd3 (_,b,_) = b

thrd3 :: (a,b,c) -> c
thrd3 (_,_,c) = c

{-
antipartitionEither :: ([a],[b]) -> [Either a b]
antipartitionEither (a, b) = undefined

filterToEither :: [Option a b c] -> [Either a b]
filterToEither x = antipartitionEither (a,b)
    where
        a = fst3 $ partitionOptions x
        b = snd3 $ partitionOptions x
-}

filterToEither :: [Option a b c] -> [Either a b]
filterToEither []               = []
filterToEither (OLeft x : xs)   = Left x : filterToEither xs
filterToEither (OMiddle x : xs) = Right x : filterToEither xs
filterToEither (_ : xs)         = filterToEither xs

eitherOptionP :: Alternative m => m a -> m b -> m c -> m (Option a b c)
eitherOptionP a b c = (OLeft <$> a) <|> (OMiddle <$> b) <|> (ORight <$> c)

-- Either utility functions

eitherId = either id id

eitherIdL = either id

eitherIdR f = either f id

-- very bad function

replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex i x xs = take i xs ++ [x] ++ drop (i + 1) xs