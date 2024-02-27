{-# LANGUAGE LambdaCase, ViewPatterns, GADTs  #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module CoreGATDs where

import Data.Maybe (fromJust)

type Name = String

data Type
    = TArr Type Type
    | TInt
    | TBool
    deriving (Show, Eq)

data Literal
    = LInt Integer
    | LBool Bool
    deriving Show

data Tm where
    Var   :: Name    -> Tm
    Lam   :: Name    -> Type -> Tm -> Tm
    App   :: Tm      -> Tm   -> Tm
    Let   :: Name    -> Tm   -> Tm -> Tm
    Lit   :: Literal -> Tm
    deriving Show

--Operators : and builtin functions: +, *, factor, ... 
{-
Builtin :: BuiltInType -> [Tm] -> Tm
--vagy 
BinOP :: Op      -> Tm   -> Tm -> Tm
UnOP  :: Op      -> Tm   -> Tm
...
-}

type Env = [(Name, (Val, Type))]

data Val where
    VVar :: Name    -> Val
    VApp :: Val     -> Val  -> Val
    VLam :: Name    -> (Val -> Val) -> Val
    VLit :: Literal -> Val

{-
-Lam:

Lam n _ e -> VLam n (\u -> evalTerm ((n, u):env) e)

TLam n t e     -> do
        t' <- typeCheck ((n,t):env) e
        return $ TArr t t'
-}

vLamApp :: Val -> Val -> Val
vLamApp (VLam _ t) u = t u
vLamApp t          u = VApp t u

evalTerm :: Env -> Tm -> Maybe (Val, Type)
evalTerm env = \case
    Var n  -> lookup n env
    Lam n t e -> undefined
    App e1 e2   -> do
        (e1', t1) <- evalTerm env e1
        (e2', t2) <- evalTerm env e2
        case t1 of
            (TArr t t') | t == t2 -> return (vLamApp e1' e2', t')
            _                     -> Nothing
    Let n e1 e2  -> do
        e1' <- evalTerm env e1
        evalTerm ((n, e1'):env) e2
    Lit l -> case l of
        (LInt i)  -> return (VLit (LInt i) , TInt)
        (LBool b) -> return (VLit (LBool b), TBool)

normalForm :: Env -> Tm -> Maybe Tm
normalForm env tm = do
    (nf, _) <- evalTerm env tm
    return $ quoteTerm (map (\x -> (fst x, snd $ snd x)) env) nf

--- Show

freshName :: [Name] -> Name -> Name
freshName ns x = if elem x ns
                    then freshName ns (x ++ "'")
                    else x

quoteTerm :: [(Name,Type)] -> Val -> Tm
quoteTerm ns (VVar n)   = Var n
quoteTerm ns (VApp e u) = App (quoteTerm ns e) (quoteTerm ns u)
quoteTerm ns (VLit l)   = Lit l
quoteTerm ns (VLam n f) = Lam x t (quoteTerm ((x,t):ns) (f (VVar x)))
    where
        x = freshName (map fst ns) n
        t = fromJust $ lookup n ns

instance Show Val where
    show = show . quoteTerm []