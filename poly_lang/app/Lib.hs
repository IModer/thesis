{-# LANGUAGE PatternSynonyms #-}

module Lib where

import Control.Applicative

-- Error type for typechecker

type Error = Either String

throwError :: String -> Either String a
throwError = Left

pattern Error :: String -> Error a
pattern Error s = Left s

pattern Result :: a -> Error a
pattern Result a = Right a

-- 3Option for Parser

data Option a b c = OLeft a | OMiddle b | ORight c
    deriving (Show, Eq)

optionElim :: (a -> d) -> (b -> d) -> (c -> d) -> Option a b c -> d
optionElim f g h o = case o of
    OLeft a   -> f a
    OMiddle b -> g b
    ORight c  -> h c

isOLeft :: Option a b c -> Bool
isOLeft = optionElim (const True) (const False) (const False)

isOMiddle :: Option a b c -> Bool
isOMiddle = optionElim (const False) (const True) (const False)

isORight :: Option a b c -> Bool
isORight = optionElim (const False) (const False) (const True)

filterToEither :: [Option a b c] -> [Either a b]
filterToEither []               = []
filterToEither (OLeft x : xs)   = Left x : filterToEither xs
filterToEither (OMiddle x : xs) = Right x : filterToEither xs
filterToEither (_ : xs)         = filterToEither xs

eitherOptionP :: Alternative m => m a -> m b -> m c -> m (Option a b c)
eitherOptionP a b c = (OLeft <$> a) <|> (OMiddle <$> b) <|> (ORight <$> c)