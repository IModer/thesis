{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Core where

import Data.Maybe

type Name = String

{-
-- So far: STLC with
    Bool and Int types
    Let bindings
    with buildin functions for Bool and Nat: (+), (*), And, Or

-- TODO :
    - More basetypes : Rac, Real, Comp
    - And subtyped typchecking
    - More tests:
        - for Bool, Int
        - for just general lambdacalc (like well typed combinators)

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
    | TPlus TTm TTm         -- (i: Int)   +  (j : Int)
    | TTimes TTm TTm        -- (i: Int)   *  (j : Int)
    | TAnd  TTm TTm         -- (b : Bool) && (l :Bool)
    | TOr  TTm TTm          -- (b : Bool) || (l :Bool)
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
typeCheck env = \case
    TLit (LInt  a) -> Just TInt
    TLit (LBool a) -> Just TBool
    TVar x         -> (lookup x env)
    TLet x e u     -> do
        t <- typeCheck env e
        typeCheck ((x,t):env) u
    TLam x t e     -> do
        t' <- typeCheck ((x,t):env) e
        return $ TArr t t'
    TApp e1 e2     -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case t1 of
            (TArr t t') | t == t2 -> return t'
            (TArr t _)            -> Nothing
            ty                    -> Nothing
    TPlus e1 e2    -> bothTypesEqual env e1 e2 TInt
    TTimes e1 e2   -> bothTypesEqual env e1 e2 TInt
    TAnd e1 e2     -> bothTypesEqual env e1 e2 TBool
    TOr e1 e2      -> bothTypesEqual env e1 e2 TBool

bothTypesEqual :: TEnv -> TTm -> TTm -> Type -> Maybe Type
bothTypesEqual env e1 e2 t  = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    if t1 == t then (if t2 == t then Just t else Nothing) else Nothing

loseType :: TTm -> Tm
loseType = \case
    TVar n     -> Var n
    TLam n t e -> Lam n (loseType e)
    TApp t u   -> App (loseType t) (loseType u)
    TLet n t u -> Let n (loseType t) (loseType u)
    TLit l     -> Lit l
    TPlus t u  -> Plus (loseType t) (loseType u)
    TAnd t u   -> And (loseType t) (loseType u)
    TTimes t u -> Times (loseType t) (loseType u)
    TOr t u    -> Or (loseType t) (loseType u)

--- Evaluation

data Tm
    = Var Name
    | Lam Name Tm
    | App Tm Tm
    | Let Name Tm Tm
    | Lit Literal
    | Plus Tm Tm
    | Times Tm Tm
    | And Tm Tm
    | Or Tm Tm
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

freshName :: [Name] -> Name -> Name
freshName ns x = if elem x ns
                    then freshName ns (x ++ "'")
                    else x

vLamApp :: Val -> Val -> Val
vLamApp (VLam _ t) u = t u
vLamApp t          u = VApp t u

evalTerm :: Env -> Tm -> Val
evalTerm env = \case
    Var n                                   -> fromJust $ lookup n env -- NOTE: fromJust is safe
    App t u                                 -> vLamApp (evalTerm env t) (evalTerm env u)
    Lam n t                                 -> VLam n (\u -> evalTerm ((n, u):env) t)
    Let n t u                               -> evalTerm ((n, evalTerm env t):env) u
    Lit l                                   -> VLit l
    Plus  (Lit (LInt i))   (Lit (LInt j))   -> VLit $ LInt $ i + j
    Times (Lit (LInt i))   (Lit (LInt j))   -> VLit $ LInt $ i * j
    And   (Lit (LBool b1)) (Lit (LBool b2)) -> VLit $ LBool $ b1 && b2
    Or    (Lit (LBool b1)) (Lit (LBool b2)) -> VLit $ LBool $ b1 || b2
    ty                                      -> error "You shouldn't be here ..."

-- Here be builtin functions

quoteTerm :: [Name] -> Val -> Tm
quoteTerm ns = \case
    VVar x                      -> Var x
    VApp t u                    -> App (quoteTerm ns t) (quoteTerm ns u)
    VLit l                      -> Lit l
    VLam (freshName ns -> x) t  -> Lam x (quoteTerm (x:ns) (t (VVar x)))

normalForm :: Env -> Tm -> Tm
normalForm env tm = quoteTerm (map fst env) $ evalTerm env tm

{-
runTypedTerm :: TTm -> Maybe Tm
runTypedTerm tm = case typeCheck [] tm of
    Just t  -> Just $ normalForm [] $ loseType tm
    Nothing -> Nothing
-}

runTypedTerm :: TTm -> Maybe Tm
runTypedTerm tm = do
    t <- typeCheck [] tm
    return $ normalForm [] $ loseType tm

--- Pritty print

prettyPrint :: Tm -> String
prettyPrint = \case
    Var n         -> n
    App t u       -> unwords ["( ",prettyPrint t," ",prettyPrint u," )"]
    Lam n t       -> unwords ["(f ",n," -> ",prettyPrint t," )"]
    Let n t u     -> unwords ["(let ",n," = ",prettyPrint t," ",prettyPrint u," )"]
    Lit (LBool l) -> show l
    Lit (LInt l)  -> show l

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

addTest x y = (TPlus (TLit (LInt x)) (TLit (LInt y)))

a = TApp (TLam "x" TBool (TLit (LBool True))) (TLit (LBool False))

b = typeCheck [] a

unreachableCodeHasBeenReached = normalForm [] (Plus (Lit (LInt 3)) (Lit (LBool True)) )