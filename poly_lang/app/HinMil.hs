module HinMil where

import Data.List

type Name = String

newtype TVar = TV Name
    deriving (Show, Eq)

data Type
    = TVar TVar
    | TArr Type Type
    --other type constructors can be here 
    deriving (Show, Eq)

data Scheme
    = Type Type
    | Forall [TVar] Type
    deriving (Show, Eq)

type Env = [(Name, Scheme)]

freeTypeVar :: Scheme -> [TVar]
freeTypeVar (Type (TVar v))    = [v]
freeTypeVar (Type (TArr t t')) = concatMap (freeTypeVar . Type) ts
freeTypeVar (Forall ns t)      = (\\) ns $ freeTypeVar (Type t)

freeTypeVarInEnv :: Env -> [TVar]
freeTypeVarInEnv = concatMap (freeTypeVar . snd)