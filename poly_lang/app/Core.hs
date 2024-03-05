{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Core where

import Data.Maybe
import Control.Monad.Trans       --lift
import Control.Monad.State.Class --MonadClass
import Control.Monad.State.Lazy  --StateT
import Data.Functor.Identity

type Name = String

type TEnv = [(Name, Type)]
type Env  = [(Name, Val)]

insertType :: (Name, Type) -> SEnv -> SEnv
insertType xt (SEnv tenv env) = SEnv (xt : tenv) env

insertName :: (Name, Val) -> SEnv -> SEnv
insertName nv (SEnv tenv env) = SEnv tenv (nv : env)

data SEnv = SEnv {typeEnv :: TEnv , 
                  nameEnv :: Env}

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
    | TTop
    deriving (Show, Eq)

runTypedTerm :: TTm -> StateT SEnv Maybe Tm
runTypedTerm tm = do
    t <- typeCheck tm
    (state . runState) $ normalForm (loseType tm) -- Unbox and box (StateT Id -> StateT Maybe)

--- Type Checking ---

-- TODO
typeCheck :: TTm -> StateT SEnv Maybe Type
typeCheck = \case
    TLit (LInt  _) -> return TInt
    TLit (LBool _) -> return TBool
    TLit LTop      -> return TTop
    TVar x         -> do
        env <- get
        lift $ lookup x (typeEnv env)  --this is same as : t <- ... ; return t
    TLet x e u     -> do
        t <- typeCheck e
        modify $ insertType (x,t)
        typeCheck u
    TLam x t e     -> do
        modify $ insertType (x,t)
        t' <- typeCheck e
        return $ TArr t t'
    TApp e1 e2     -> do
        t1 <- typeCheck e1
        t2 <- typeCheck e2
        case t1 of
            (TArr t t') | t == t2 -> return t'
--            These cases will be hanfles by Maybe MonadFail being Nothing
--            (TArr _ _)            -> Nothing
--            _                     -> Nothing
    TPlus e1 e2    -> bothTypesEqual e1 e2 TInt
    TTimes e1 e2   -> bothTypesEqual e1 e2 TInt
    TAnd e1 e2     -> bothTypesEqual e1 e2 TBool
    TOr e1 e2      -> bothTypesEqual e1 e2 TBool

--bothTypesEqual :: TEnv -> TTm -> TTm -> Type -> Maybe Type
--bothTypesEqual :: TTm -> TTm -> TEnv -> (TEnv, Maybe Type)
bothTypesEqual :: TTm -> TTm -> Type -> StateT SEnv Maybe Type
bothTypesEqual e1 e2 t  = do
    t1 <- typeCheck e1
    t2 <- typeCheck e2
    -- This should be Nothing if its False becasue Maybe monad has Nothing as fail
    -- https://hackage.haskell.org/package/base-4.19.1.0/docs/src/Control.Monad.Fail.html#line-74
    case t1 == t && t2 == t of
        True -> return t

loseType :: TTm -> Tm
loseType = \case
    TVar n     -> Var n
    TLam n _ e -> Lam n (loseType e)
    TApp t u   -> App (loseType t) (loseType u)
    TLet n t u -> Let n (loseType t) (loseType u)
    TLit l     -> Lit l
    TPlus t u  -> Plus (loseType t) (loseType u)
    TAnd t u   -> And (loseType t) (loseType u)
    TTimes t u -> Times (loseType t) (loseType u)
    TOr t u    -> Or (loseType t) (loseType u)

--- Evaluation ---

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

showTm :: Tm -> String
showTm = \case
    Var n         -> n
    App t u       -> unwords ["(",showTm t,showTm u,")"]
    Lam n t       -> unwords ["(\\",n,"->",showTm t,")"]
    Let n t u     -> unwords ["(let",n,"=",showTm t,showTm u,")"]
    Lit (LBool l) -> show l
    Lit (LInt l)  -> show l
    Lit LTop      -> "()"  --TODO : we can make this empty, its only () for debug
    Plus t u      -> unwords [showTm t,"+",showTm u]
    Times t u     -> unwords [showTm t,"+",showTm u]
    And t u       -> unwords [showTm t,"+",showTm u]
    Or t u        -> unwords [showTm t,"+",showTm u]

instance Show Tm where
    show = showTm

data Literal
    = LInt Int
    | LBool Bool
    | LTop
    deriving Show

data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Val)
    | VLit Literal

freshName :: [Name] -> Name -> Name
freshName ns x = if x `elem` ns
                    then freshName ns (x ++ "'")
                    else x

-- TODO
vLamApp :: Val -> Val -> Val
vLamApp (VLam _ t) u = t u
vLamApp t          u = VApp t u

{-  This might be the hardest, it should be : VLam (\u -> do modify insert (n,u);evalTerm t)
    Lam n t                                 -> do
        modify $ insert (n, u)
        VLam n (\u -> evalTerm t)
        --VLam n (\u -> evalTerm ((n, u):env) t)
-}

-- TODO
--evalTerm :: Env -> Tm -> Val
evalTerm :: Tm -> State SEnv Val
evalTerm = \case
    Var n     -> do
        env <- get
        return $ fromJust $ lookup n $ nameEnv env -- NOTE: fromJust is safe
    App t u   -> do
        env <- get
        t' <- evalTerm t
        u' <- evalTerm u 
        return $ vLamApp t' u'
    Lam n t   -> do
        env <- get
        return $ VLam n (\u -> evalState (do
            modify (insertName (n, u))
            evalTerm t) env)
    Let n e u -> do
        e' <- evalTerm e
        modify $ insertName (n , e')
        evalTerm u
        --evalTerm ((n, evalTerm env t):env) u
    Lit l     -> return $ VLit l
    --TODO :  Le kell kezelni hogy mivan ha az egyik érték változó
    Plus e u  -> do
        e' <- evalTerm e
        u' <- evalTerm u
        return $ VLit $ LInt $ isBothInt e' u' (+)
    Times e u -> do
        e' <- evalTerm e
        u' <- evalTerm u
        return $ VLit $ LInt $ isBothInt e' u' (*)
    And e u   -> do
        e' <- evalTerm e
        u' <- evalTerm u
        return $ VLit $ LBool $ isBothBool e' u' (&&)
    Or e u    -> do
        e' <- evalTerm e
        u' <- evalTerm u
        return $ VLit $ LBool $ isBothBool e' u' (||)

isBothBool :: Val -> Val -> (Bool -> Bool -> Bool) -> Bool
isBothBool v v' f = case v of
    VLit (LBool i) -> case v' of 
        VLit (LBool j) -> f i j
        _              -> error "..."
    _              -> error "..."

isBothInt :: Val -> Val -> (Int -> Int -> Int) -> Int
isBothInt v v' f = case v of
    VLit (LInt i) -> case v' of
        VLit (LInt j) -> f i j
        _              -> error "..."
    _              -> error "..."

-- Here be builtin functions

-- TODO
--quoteTerm :: Val -> State SEnv Tm
quoteTerm :: [Name] -> Val -> Tm
quoteTerm ns = \case
    VVar x                      -> Var x
    VApp t u                    -> App (quoteTerm ns t) (quoteTerm ns u)
    VLit l                      -> Lit l
    VLam (freshName ns -> x) t  -> Lam x (quoteTerm (x:ns) (t (VVar x)))

-- TODO
normalForm :: Tm -> State SEnv Tm
normalForm tm = do
    val <- evalTerm tm
    env <- get
    lift $ Identity $ quoteTerm (map fst $ nameEnv env) val 