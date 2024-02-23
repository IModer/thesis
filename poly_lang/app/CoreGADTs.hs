{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module CoreGATDs where

--Ask how to index Tm with types
{-
data TypeK where
    TArr  :: TypeK -> TypeK -> TypeK
    TInt  :: TypeK
    TBool :: TypeK
    deriving (Show, Eq)
-}

type Name = String

data Type
    = TArr Type Type
    | TInt
    | TBool
    deriving (Show, Eq)

data Literal
    = LInt Int
    | LBool Bool

data Tm (t :: Type) where
    Var :: Name -> Tm t -> Tm t
    Lam :: Name -> Type -> Tm t
    App :: Tm t -> Tm t -> Tm t
    Let :: Name -> Tm  -> Tm t
    Lit :: Literal -> Tm t
    