{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE LambdaCase, ViewPatterns, OverloadedStrings, PatternSynonyms #-}
module Core where

import Data.Maybe
import Control.Monad.Trans       --lift
import Control.Monad.State.Class --MonadClass
import Control.Monad.State.Lazy  --StateT
import Data.Functor.Identity
import qualified Data.Text as T
import Ring hiding (mod, div)

--type Name = String
type Name = T.Text

type TEnv = [(Name, Type)]
type Env  = [(Name, Val)]

insertType :: (Name, Type) -> SEnv -> SEnv
insertType xt (SEnv tenv env) = SEnv (xt : tenv) env

insertName :: (Name, Val) -> SEnv -> SEnv
insertName nv (SEnv tenv env) = SEnv tenv (nv : env)

data SEnv = SEnv {typeEnv :: TEnv , 
                  nameEnv :: Env}

data TTm
    = TVar    Name
    | TLam    Name Type TTm
    | TApp    TTm  TTm
    | TLet    Name TTm  TTm
    | TLit    Literal
    | TBinOp  BinOp TTm TTm
    | TNeg    TTm
    deriving Show

data BinOp
    = And
    | Or
    | Eq
    | Plus
    | Times
    | Minus
    | Div
    | IntDiv
    | Mod
    | Pow
    | Lte
    | Gte
    | Lt
    | Gt
    deriving Eq

instance Show BinOp where
    show op = case op of
        And     -> "&"
        Or      -> "|"
        Eq      -> "="
        Plus    -> "+"
        Times   -> "*"
        Minus   -> "-"
        Div     -> "/"
        IntDiv  -> "div"
        Mod     -> "mod"
        Pow     -> "^"
        Lte     -> "<="
        Gte     -> ">="
        Lt      -> "<"
        Gt      -> ">"

data Type
    = TArr Type Type
    | TNumber
    | TBool
    | TTop
    deriving (Show, Eq)

runTypedTerm :: TTm -> StateT SEnv Maybe Tm
runTypedTerm tm = do
    t <- typeCheck [] tm
    (state . runState) $ normalForm (loseType tm) -- Unbox and box (StateT Id -> StateT Maybe)

--- Type Checking ---
--           TEnv is Context for lambdas
typeCheck :: TEnv -> TTm -> StateT SEnv Maybe Type
typeCheck env = \case
    TLit (LNumber  _) -> return TNumber
    TLit (LBool _) -> return TBool
    TLit LTop      -> return TTop
    TVar x         -> do
        env' <- get
        maybe (lift $ lookup x env) return (lookup x (typeEnv env')) --this is same as : t <- ... ; return t
    TLet x e u     -> do
        t <- typeCheck env e
        modify $ insertType (x,t)
        typeCheck env u
    TLam x t e     -> do 
        t' <- typeCheck ((x,t):env) e
        return $ TArr t t'
    TApp e1 e2     -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case t1 of
            (TArr t t') | t == t2 -> return t'
            _                     -> lift Nothing
    TBinOp op e1 e2 -> if op `elem` [And, Or]
        then bothTypesEqual env e1 e2 TBool
        else bothTypesEqual env e1 e2 TNumber
    TNeg e          -> do
        t <- typeCheck env e
        case t of
            TNumber -> return TNumber
            _       -> lift Nothing

bothTypesEqual :: TEnv -> TTm -> TTm -> Type -> StateT SEnv Maybe Type
bothTypesEqual env e1 e2 t  = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    if t1 == t && t2 == t then
        return t
    else
        lift Nothing

loseType :: TTm -> Tm
loseType = \case
    TVar n        -> Var n
    TLam n _ e    -> Lam n (loseType e)
    TApp t u      -> App (loseType t) (loseType u)
    TLet n t u    -> Let n (loseType t) (loseType u)
    TLit l        -> Lit l
    TNeg t        -> Neg (loseType t)
    TBinOp op t u -> BinOp op (loseType t) (loseType u)

--- Evaluation ---

data Tm
    = Var Name
    | Lam Name Tm
    | App Tm Tm
    | Let Name Tm Tm
    | Lit Literal
    | Neg Tm
    | BinOp BinOp Tm Tm
--    deriving Show

showTm :: Tm -> String
showTm = \case
    Var n         -> T.unpack n
    App t u       -> unwords ["(",showTm t,showTm u,")"]
    Lam n t       -> unwords ["(\\",T.unpack n,"->",showTm t,")"]
    Let n t u     -> unwords ["(let",T.unpack n,"=",showTm t,showTm u,")"]
    Lit (LBool l) -> show l
    Lit (LNumber l)  -> show l
    Lit LTop      -> "()"  --TODO : we can make this empty, its only () for debug
    Neg    t      -> unwords ["-",showTm t]
    BinOp op t u  -> unwords [showTm t,show op,showTm u]

instance Show Tm where
    show = showTm

data Literal
    = LNumber Number -- Ring a
    | LBool Bool
    | LTop
    deriving Show

-- Operators/Functions should also have Val : like +, *, ...
data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Val)
    | VLit Literal
    | VNeg Val --this might not be needed
    | VBinOp BinOp Val Val

pattern VBool b = VLit (LBool b)
pattern VNumber n = VLit (LNumber n)


freshName :: [Name] -> Name -> Name
freshName ns x = if x `elem` ns
                    then freshName ns $ T.snoc x '\'' 
                    else x

vLamApp :: Val -> Val -> Val
vLamApp (VLam _ t) u = t u
vLamApp t          u = VApp t u

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
    Lit l     -> return $ VLit l
    Neg e     -> do
        e' <- evalTerm e
        return $ case e' of 
            (VNumber i) -> VNumber (- i) -- TODO maybe other cases need to be handled?
    BinOp op e u -> do
        e' <- evalTerm e
        u' <- evalTerm u
        return $ case (op , e', u') of
            (And    , VBool i   , VBool j  ) -> VBool   $ i && j
            (Or     , VBool i   , VBool j  ) -> VBool   $ i || j
            (Eq     , VBool i   , VBool j  ) -> VBool   $ i == j
            (Eq     , VNumber i , VNumber j) -> VBool   $ i == j
            (Plus   , VNumber i , VNumber j) -> VNumber $ i + j
            (Times  , VNumber i , VNumber j) -> VNumber $ i * j
            (Minus  , VNumber i , VNumber j) -> VNumber $ i - j
            (Div    , VNumber i , VNumber j) -> VNumber $ i `div` j -- TODO solve this
            (IntDiv , VNumber i , VNumber j) -> VNumber $ i `div` j  
            (Mod    , VNumber i , VNumber j) -> VNumber $ i `mod` j
            (Pow    , VNumber i , VNumber j) -> VNumber $ i ^ j
            (Lte    , VNumber i , VNumber j) -> VBool   $ i <= j
            (Gte    , VNumber i , VNumber j) -> VBool   $ i >= j
            (Lt     , VNumber i , VNumber j) -> VBool   $ i < j
            (Gt     , VNumber i , VNumber j) -> VBool   $ i > j
            (_      ,   a       , b        ) -> VBinOp op a b

quoteTerm :: [Name] -> Val -> Tm
quoteTerm ns = \case
    VVar x                      -> Var x
    VApp t u                    -> App (quoteTerm ns t) (quoteTerm ns u)
    VLit l                      -> Lit l
    VLam (freshName ns -> x) t  -> Lam x (quoteTerm (x:ns) (t (VVar x)))
    VNeg t                      -> Neg (quoteTerm ns t)
    VBinOp op l r               -> BinOp op (quoteTerm ns l) (quoteTerm ns r)

normalForm :: Tm -> State SEnv Tm
normalForm tm = do
    val <- evalTerm tm
    env <- get
    lift $ Identity $ quoteTerm (map fst $ nameEnv env) val