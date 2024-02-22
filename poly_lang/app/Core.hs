module Core where

import Data.Maybe

type Name = String

{-
-- TODO : 
    - builting functions for Int Bool, (+), (*), ...
    - More basetypes : Int, Rac, Real, ...
    - More tests : 
        - for Bool, Int
        - for just general lambdacalc (like well typed combinators)

    - Maybe : merge TTm and Tm
-}

-- FOR NOW: trick:
--  TTm is a typed term, 
--  we type check it then we translate it into Tm

data TTm
    = TVar Name
    | TLam Name Type TTm
    | TApp TTm TTm
    | TLet Name TTm TTm
    | TLit Literal
    deriving Show

data Type 
    = TArr Type Type
    | TInt
    | TBool
    deriving (Show, Eq)

type TEnv = [(Name, Type)]

--- Type checking

-- TODO: Proper Error monad instead of Maybe
typeCheck :: TEnv -> TTm -> Maybe Type
typeCheck _ (TLit (LInt  a)) = Just TInt
typeCheck _ (TLit (LBool a)) = Just TBool
typeCheck env (TVar x) = (lookup x env)
typeCheck env (TLam x t e) = case (typeCheck ((x,t):env) e) of
    Just t' -> Just $ TArr t t'
    Nothing -> Nothing
typeCheck env (TApp e1 e2) = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    case t1 of
        (TArr t t') | t == t2 -> return t'
        (TArr t _)            -> Nothing
        ty                    -> Nothing

loseType :: TTm -> Tm
loseType (TVar n)     = Var n
loseType (TLam n t e) = Lam n (loseType e)
loseType (TApp t u)   = App (loseType t) (loseType u)
loseType (TLet n t u) = Let n (loseType t) (loseType u)
loseType (TLit l)     = Lit l

---

data Tm 
    = Var Name
    | Lam Name Tm
    | App Tm Tm
    | Let Name Tm Tm
    | Lit Literal
    deriving Show

data Literal
    = LInt Int
    | LBool Bool 
    deriving Show

type Env = [(Name, Val)]

data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Val)
    | VLit Literal

--- Evaluation

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
evalTerm env (Lit l)     = VLit l
-- Here be builtin functions

quoteTerm :: [Name] -> Val -> Tm
quoteTerm _  (VVar x)     = Var x
quoteTerm ns (VApp t u)   = App (quoteTerm ns t) (quoteTerm ns u)
quoteTerm ns (VLit l)     = Lit l
quoteTerm ns (VLam x t)   = Lam fx (quoteTerm (fx:ns) (t (VVar x)))
    where
        fx = freshName ns x

normalForm :: Env -> Tm -> Tm
normalForm env tm = quoteTerm (map fst env) $ evalTerm env tm

runTypedTerm :: TTm -> Maybe Tm
runTypedTerm tm = case typeCheck [] tm of
    Just t  -> Just $ normalForm [] $ loseType tm
    Nothing -> Nothing

--- Tests

test1 = normalForm [] (Lam "x" (App (Var "x") (Var "x")) )

idTm = (Lam "x" (Var "x"))

-- Only for Untyped cos in STLC Y is not well typed
--yTm = (Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "f") (App (Var "x") (Var "x"))))))

test2 = (App idTm test1)

test3 = (Let "x" (idTm) (App test1 (Var "x")))

boolId = (TLam "b" TBool (TVar "b")) -- :: TArr TBool TBool 

true = TLit (LBool True)             -- :: TBool 

trueAgain = TApp boolId true         -- :: TBool

runTrueAgain = runTypedTerm trueAgain -- == Just (Lit (LBool True))