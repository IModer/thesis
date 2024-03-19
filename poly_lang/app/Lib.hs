module Lib where

import Control.Applicative

-- 3Option for Parser

data Option a b c = OLeft a | OMiddle b | ORight c
    deriving (Show, Eq)

optionDest :: (a -> d) -> (b -> d) -> (c -> d) -> Option a b c -> d
optionDest f g h o = case o of
    OLeft a   -> f a
    OMiddle b -> g b
    ORight c  -> h c

eitherOptionP :: Alternative m => m a -> m b -> m c -> m (Option a b c)
eitherOptionP a b c = (OLeft <$> a) <|> (OMiddle <$> b) <|> (ORight <$> c)