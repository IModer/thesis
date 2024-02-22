module Core where

import Data.Maybe

type Name = String

{-
-- De Bruijn index
newtype Ix  = Ix  Int 
    deriving (Eq, Show, Num) via Int

-- De Bruijn level
newtype Lvl = Lvl Int 
    deriving (Eq, Show, Num) via Int
-}

data Tm 
    = Var Name
--    | Lam Name Type Tm
    | Lam Name Tm
    | App Tm Tm
    | Let Name Tm Tm
    | Lit Literal
    deriving Show

data Literal
    = LInt Int
    | LBool Bool 
    deriving Show

data Type 
    = TArr Type Type
    | TInt
    | TBool
    deriving Show

type Env = [(Name, Val)]

data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Val)

freshName :: [Name] -> Name -> Name
freshName ns x = if elem x ns 
                    then freshName ns (x ++ "'") 
                    else x

vLamApp :: Val -> Val -> Val
vLamApp (VLam _ t) u = t u
vLamApp t          u = VApp t u

evalTerm :: Env -> Tm -> Val
evalTerm env (Var n)     = fromJust $ lookup n env  --TODO : not fromJust
evalTerm env (App t u)   = vLamApp (evalTerm env t) (evalTerm env u)
evalTerm env (Lam n t)   = VLam n (\u -> evalTerm ((n, u):env) t)
evalTerm env (Let n t u) = evalTerm ((n, evalTerm env t):env) u

quoteTerm :: [Name] -> Val -> Tm
quoteTerm ns (VVar x)     = Var x
quoteTerm ns (VApp t u)   = App (quoteTerm ns t) (quoteTerm ns u)
--Question what goes here ^
quoteTerm ns (VLam x t)   = Lam fx (quoteTerm (fx:ns) (t (VVar x)))
    where
        fx = freshName ns x

normalForm :: Env -> Tm -> Tm
normalForm env tm = quoteTerm (map fst env) $ evalTerm env tm

-- Tests
test1 = normalForm [] (Lam "x" (App (Var "x") (Var "x")) )

idTm = (Lam "x" (Var "x"))

-- Only for Untyped cos in STLC Y is not well typed
--yTm = (Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "f") (App (Var "x") (Var "x"))))))

test2 = (App idTm test1)

test3 = (Let "x" (idTm) (App test1 (Var "x")))