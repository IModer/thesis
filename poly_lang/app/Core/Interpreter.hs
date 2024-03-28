{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Core.Interpreter where

import Core.AST
import Core.TypeChecker
import Core.Classes

import Data.Maybe
import Control.Monad.Trans       --lift
import Control.Monad.State.Class --MonadClass
import Control.Monad.State.Lazy  --StateT
import Data.Functor.Identity
import Data.Text hiding (map, elem)
import Ring

runTypedTerm :: TTm -> ErrorT GState Tm
runTypedTerm tm = do
    _ <- typeCheck [] tm
    lift $ normalForm (loseType tm)

--- Evaluation ---

freshName :: [Name] -> Name -> Name
freshName ns x = if x `elem` ns
                    then freshName ns $ snoc x '\'' 
                    else x

vLamApp :: Val -> Val -> Val
vLamApp (VLam _ t) u = t u
vLamApp t          u = VApp t u

evalTerm :: VEnv -> Tm -> State GEnv Val
evalTerm env' = \case
    Var n     -> do
        env <- get
        maybe   (return $ fromJust $ lookup n $ getVal env) 
                return
                (lookup n env')
    App t u   -> do
        t' <- evalTerm env' t
        u' <- evalTerm env' u
        return $ vLamApp t' u'
    Lam n t   -> do
        env <- get
        return $ VLam n (\u -> evalState (evalTerm ((n, u):env') t) env )
    IfThenElse b t u -> do
        b' <- evalTerm env' b
        case b' of
            (VBool True ) -> evalTerm env' t
            (VBool False) -> evalTerm env' u
            _             -> undefined
    Let n e u -> do
        e' <- evalTerm env' e
        evalTerm ((n, e'):env') u
    Lit l     -> return $ VLit l
    Prefix op e -> do
        e' <- evalTerm env' e
        return $ case (op , e') of
            (Neg    , VNumber i) -> VNumber (- i)
            (Factor , VNumber i) -> VNumber (factor i)
            (Irred  , VNumber i) -> VBool (irred i)
            (Der    , VNumber i) -> VNumber (derivative i)
            (_      , a        ) -> VPrefix op a
    BinOpPoly op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            (VPoly i, VPoly j) -> VPoly $ i `f` j
            (a      , b      ) -> VBinOpPoly op f a b
    BinOpBool op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            (VBool i, VBool j) -> VBool $ i `f` j
            (a      , b      ) -> VBinOpBool op f a b
    BinOpNum op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            (VNumber i, VNumber j) -> VNumber $ i `f` j
            (a      , b      )     -> VBinOpNum op f a b
    -- Here we want to control what output type we have and so on...
    BinOp op e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (op , e', u') of
            (Eq     , VBool i   , VBool j  ) -> VBool   $ i == j
            (Eq     , VNumber i , VNumber j) -> VBool   $ i == j
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
    VBinOpPoly op f l r         -> BinOpPoly op f (quoteTerm ns l) (quoteTerm ns r)
    VBinOpBool op f l r         -> BinOpBool op f (quoteTerm ns l) (quoteTerm ns r)
    VBinOpNum op f l r          -> BinOpNum op f (quoteTerm ns l) (quoteTerm ns r)

normalForm :: Tm -> State GEnv Tm
normalForm tm = do
    val <- evalTerm [] tm
    env <- get
    lift $ Identity $ quoteTerm (map fst $ getVal env) val