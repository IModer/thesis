{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE LambdaCase, ViewPatterns, OverloadedStrings, PatternSynonyms #-}
module Core.Interpreter where

import Lib
import Core.AST
import Core.TypeChecker

import Data.Maybe
import Control.Monad.Trans       --lift
import Control.Monad.State.Class --MonadClass
import Control.Monad.State.Lazy  --StateT
import Data.Functor.Identity
import qualified Data.Text as T
import Ring hiding (mod, div)

runTypedTerm :: TTm -> StateT SEnv Error Tm
runTypedTerm tm = do
    _ <- typeCheck [] tm
    (state . runState) $ normalForm (loseType tm) -- Unbox and box (StateT Id -> StateT Maybe)

--- Evaluation ---

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
    -- Here we want to comtrol what output type we have and so on...
    BinOp op e u -> do
        e' <- evalTerm e
        u' <- evalTerm u
        return $ case (op , e', u') of
            (Eq     , VBool i   , VBool j  ) -> VBool   $ i == j
            (Eq     , VNumber i , VNumber j) -> VBool   $ i == j
            (Div    , VNumber i , VNumber j) -> VNumber $ i `div` j
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