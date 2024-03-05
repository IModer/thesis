{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Core where

import Data.Maybe
import Control.Monad.Trans       --lift
import Control.Monad.State.Class --MonadClass
import Control.Monad.State.Lazy  --StateT

type Name = String

type TEnv = [(Name, Type)]
type Env  = [(Name,TTm)]

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

eval :: String -> State SEnv String 
eval cs = case parseString cs of
    Left a   -> return "Parse error"        -- TODO : print errors
    Right tm -> helper (runTypedTerm' tm)

        where
            -- TODO redo this
            helper :: StateT SEnv Maybe Tm -> State SEnv String
            helper st = do
                env <- get
                mapStateT (\case 
                    (Just (tm',env')) -> return (prettyPrint tm',env')
                    Nothing -> return ("Type error",env) ) st

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
    TVar x         -> undefined
    TLet x e u     -> do
        t <- typeCheck' e
        modify (\(SEnv tenv env) -> SEnv ((x,t) : tenv) env)
        typeCheck' u
    TLam x t e     -> undefined
    TApp e1 e2     -> undefined
    TPlus e1 e2    -> undefined
    TTimes e1 e2   -> undefined
    TAnd e1 e2     -> undefined
    TOr e1 e2      -> undefined

bothTypesEqual :: TEnv -> TTm -> TTm -> Type -> Maybe Type
bothTypesEqual env e1 e2 t  = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    if t1 == t && t2 == t then Just t else Nothing

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
    deriving Show

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

-- TODO
--evalTerm :: Tm -> State SEnv Val
evalTerm :: Env -> Tm -> Val
evalTerm env = \case
    Var n                                   -> fromJust $ lookup n env -- NOTE: fromJust is safe
    App t u                                 -> vLamApp (evalTerm env t) (evalTerm env u)
    Lam n t                                 -> VLam n (\u -> evalTerm ((n, u):env) t)
    Let n t u                               -> evalTerm ((n, evalTerm env t):env) u
    Lit l                                   -> VLit l
    Plus  u                 t               -> case evalTerm env u of
        VLit (LInt i)    -> case evalTerm env t of
            VLit (LInt j)    -> VLit $ LInt $ i + j
            _                -> error "You shouldn't be here ..."
        _                -> error "You shouldn't be here ..."
    Times u                 t               -> case evalTerm env u of
        VLit (LInt i)    -> case evalTerm env t of
            VLit (LInt j)    -> VLit $ LInt $ i * j
            _                -> error "You shouldn't be here ..."
        _                -> error "You shouldn't be here ..."
    And   u                 t               -> case evalTerm env u of
        VLit (LBool i)    -> case evalTerm env t of
            VLit (LBool j)   -> VLit $ LBool $ i && j
            _                -> error "You shouldn't be here ..."
        _                -> error "You shouldn't be here ..."
    Or    u                 t               -> case evalTerm env u of
        VLit (LBool i)    -> case evalTerm env t of
            VLit (LBool j)   -> VLit $ LBool $ i || j
            _                -> error "You shouldn't be here ..."
        _                -> error "You shouldn't be here ..."

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
normalForm = undefined