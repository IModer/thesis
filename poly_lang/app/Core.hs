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

type BoolOpType = Bool -> Bool -> Bool
type NumOpType = Number -> Number -> Number

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
    | TBinOpBool BinOp BoolOpType TTm TTm
    | TBinOpNum  BinOp NumOpType TTm TTm
    | TBinOp  BinOp TTm TTm
    | TPrefix PrefixOp TTm
--    deriving Show

instance Show TTm where
    show t = show (loseType t)

data PrefixOp
    = Neg
    | Factor
    | Irred
    | Der

instance Show PrefixOp where
    show = \case
        Neg    -> "-"
        Factor -> "factor"
        Irred  -> "irred"
        Der    -> "derivative"

-- Abs
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
    show = \case
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
    | TPoly
    | TTop
    deriving (Show, Eq)

-- TODO:     :: TTm -> StateT GEnv Maybe Tm
runTypedTerm :: TTm -> StateT SEnv Maybe Tm
runTypedTerm tm = do
    _ <- typeCheck [] tm
    (state . runState) $ normalForm (loseType tm) -- Unbox and box (StateT Id -> StateT Maybe)

--- Type Checking ---
--           TEnv is Context for lambdas
--        :: TTm -> StateT (TEnv, GEnv) Maybe Type
{-
    TEnv :: [(Name, Type)]
    GEnv :: [(Name, (Type, Tm))]
    kell : 
        var x     : lookup x in TEnv or GEnv
        let x e u : t <- typecheck e; insert (e,t) into TEnv; typecheck u
        lam x t e : insert (x,t) into TEnv; typecheck e; ...
        app e1 e2 : typecheck e1 e2, ...
        binop, prefix hasonlóan

    vagyis nem insertelünk GEnv be csak a val-nál keresünk benne

-}
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
--        modify $ insertType (x,t)
        typeCheck ((x,t):env) u
    TLam x t e     -> do 
        t' <- typeCheck ((x,t):env) e
        return $ TArr t t'
    TApp e1 e2     -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case t1 of
            (TArr t t') | t == t2 -> return t'
            _                     -> lift Nothing
    -- Ezt még át kell gondolni, pl.: most true = false nem valid, pedig annak kéne lennie
    TBinOpBool _ _ e1 e2 -> bothTypesEqual env e1 e2 TBool
    TBinOpNum  _ _ e1 e2 -> bothTypesEqual env e1 e2 TNumber
    TBinOp op e1 e2 -> if op `elem` [And, Or]
        then bothTypesEqual env e1 e2 TBool
        else bothTypesEqual env e1 e2 TNumber
    TPrefix op e    -> do
        e' <- typeCheck env e
        case (op, e') of
            (Neg    , TNumber) -> return TNumber
            (Factor , TNumber) -> return TNumber
            (Der    , TNumber) -> return TNumber
            (Irred  , TNumber) -> return TBool
            (_      , _      ) -> lift Nothing

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
    TPrefix op t  -> Prefix op (loseType t)
    TBinOp op t u -> BinOp op (loseType t) (loseType u)
    TBinOpBool op f t u -> BinOpBool op f (loseType t) (loseType u)
    TBinOpNum op f t u  -> BinOpNum op f (loseType t) (loseType u)


--- Evaluation ---

data Tm
    = Var Name
    | Lam Name Tm
    | App Tm Tm
    | Let Name Tm Tm
    | Lit Literal
    | Prefix PrefixOp Tm
    | BinOp BinOp Tm Tm
    | BinOpBool BinOp BoolOpType Tm Tm
    | BinOpNum  BinOp NumOpType Tm Tm
--    deriving Show

showTm :: Tm -> String
showTm = \case
    Var n              -> T.unpack n
    App t u            -> unwords ["(",showTm t,showTm u,")"]
    Lam n t            -> unwords ["(\\",T.unpack n,"->",showTm t,")"]
    Let n t u          -> unwords ["(let",T.unpack n,"=",showTm t,showTm u,")"]
    Lit (LBool l)      -> show l
    Lit (LNumber l)    -> show l
    Lit LTop           -> "()"  --TODO : we can make this empty, its only () for debug
    Prefix op t        -> unwords [show op ,showTm t]
    BinOp op t u       -> unwords [showTm t,show op,showTm u]
    BinOpBool op _ t u -> unwords [showTm t,show op,showTm u]
    BinOpNum  op _ t u -> unwords [showTm t,show op,showTm u]

instance Show Tm where
    show = showTm

data Literal
    = LNumber Number -- Ring a
    | LBool Bool
    | LTop
    deriving Show

data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Val)
    | VLit Literal
    | VPolyVar Name
    | VPrefix PrefixOp Val
    | VBinOp BinOp Val Val
    | VBinOpBool BinOp BoolOpType Val Val
    | VBinOpNum  BinOp NumOpType Val Val

pattern VBool :: Bool -> Val
pattern VBool b = VLit (LBool b)

pattern VNumber :: Number -> Val
pattern VNumber n = VLit (LNumber n)

freshName :: [Name] -> Name -> Name
freshName ns x = if x `elem` ns
                    then freshName ns $ T.snoc x '\'' 
                    else x

vLamApp :: Val -> Val -> Val
vLamApp (VLam _ t) u = t u
vLamApp t          u = VApp t u

-- TODO: HOAS binop és prefix
{-
    GEnv :: [(Name, (Type, Tm))]
    Env  :: [(Name, Tm)]

    typecheck hez hasonlóan csak a a var lookupol 
    GEnv és Env ben, minden más csak pakol Env-be 
-}
-- TODO: :: Tm -> State (Env, GEnv)
evalTerm :: Tm -> State SEnv Val
evalTerm = \case
    Var n     -> do
        env <- get
        return $ fromJust $ lookup n $ nameEnv env -- NOTE: fromJust is safe
    App t u   -> do
--        env <- get
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
    Prefix op e -> do
        e' <- evalTerm e
        return $ case (op , e') of
            (Neg    , VNumber i) -> VNumber (- i)
            (Factor , VNumber i) -> VNumber (factor i)
            (Irred  , VNumber i) -> VBool (irred i)
            (Der    , VNumber i) -> VNumber (derivative i)
            (_      , a        ) -> VPrefix op a
    BinOpBool op f e u -> do
        e' <- evalTerm e
        u' <- evalTerm u
        return $ case (e', u') of
            (VBool i, VBool j) -> VBool $ i `f` j
            (a      , b      ) -> VBinOpBool op f a b
    BinOpNum op f e u -> do
        e' <- evalTerm e
        u' <- evalTerm u
        return $ case (e', u') of
            (VNumber i, VNumber j) -> VNumber $ i `f` j
            (a      , b      )     -> VBinOpNum op f a b
    -- Here we want to comtroll what output type we have and so on...
    BinOp op e u -> do
        e' <- evalTerm e
        u' <- evalTerm u
        return $ case (op , e', u') of
            (Eq     , VBool i   , VBool j  ) -> VBool   $ i == j
            (Eq     , VNumber i , VNumber j) -> VBool   $ i == j
            (Div    , VNumber i , VNumber j) -> VNumber $ i `div` j -- TODO solve this
            (Lte    , VNumber i , VNumber j) -> VBool   $ i <= j
            (Gte    , VNumber i , VNumber j) -> VBool   $ i >= j
            (Lt     , VNumber i , VNumber j) -> VBool   $ i < j
            (Gt     , VNumber i , VNumber j) -> VBool   $ i > j
            (_      , a         , b        ) -> VBinOp op a b

quoteTerm :: [Name] -> Val -> Tm
quoteTerm ns = \case
    VVar x                      -> Var x
    VApp t u                    -> App (quoteTerm ns t) (quoteTerm ns u)
    VLit l                      -> Lit l
    VLam (freshName ns -> x) t  -> Lam x (quoteTerm (x:ns) (t (VVar x)))
    VPrefix op l                -> Prefix op (quoteTerm ns l)
    VBinOp op l r               -> BinOp op (quoteTerm ns l) (quoteTerm ns r)
    VBinOpBool op f l r         -> BinOpBool op f (quoteTerm ns l) (quoteTerm ns r)
    VBinOpNum op f l r          -> BinOpNum op f (quoteTerm ns l) (quoteTerm ns r)
    VPolyVar n                  -> Var n

normalForm :: Tm -> State SEnv Tm
normalForm tm = do
    val <- evalTerm tm
    env <- get
    lift $ Identity $ quoteTerm (map fst $ nameEnv env) val