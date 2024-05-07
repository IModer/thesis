{-# LANGUAGE PatternSynonyms #-}

module Lib where

import Control.Applicative (Alternative(..))

-- Type with 3 options for Parser

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

filterToEither :: [Option a b c] -> [Either a b]
filterToEither []               = []
filterToEither (OLeft x : xs)   = Left x : filterToEither xs
filterToEither (OMiddle x : xs) = Right x : filterToEither xs
filterToEither (_ : xs)         = filterToEither xs

eitherOptionP :: Alternative m => m a -> m b -> m c -> m (Option a b c)
eitherOptionP a b c = (OLeft <$> a) <|> (OMiddle <$> b) <|> (ORight <$> c)

-- Either utility functions

eitherId :: Either c c -> c
eitherId = either id id

eitherIdL :: (b -> c) -> Either c b -> c 
eitherIdL = either id

eitherIdR :: (a -> c) -> Either a c -> c
eitherIdR f = either f id

-- Utulity for lists

replaceAtIndex :: Integer -> a -> [a] -> [a]
replaceAtIndex i x xs = take (fromIntegral i) xs ++ 
    [x] ++ 
    drop (fromIntegral $ i + 1) xs