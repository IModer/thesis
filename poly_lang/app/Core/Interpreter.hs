{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Core.Interpreter where

import Core.AST
import Core.TypeChecker
import Core.Classes
import Core.Types

import Data.Maybe
import Control.Monad.Trans       --lift
import Control.Monad.State.Class --MonadClass
import Control.Monad.State.Lazy  --StateT
import Data.Functor.Identity
import Data.Text hiding (map, elem)
import Prelude hiding ((*), (+), negate, (-), quot, rem, lcm, gcd)
import Data.Semiring
import Data.Euclidean

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
            (Neg    , VNum i) -> VNum (negate i)
            (Factor , VNum i) -> VNum (factor i)
            (Irred  , VNum i) -> VBool (irred i)
            (Der    , VNum i) -> VNum (derivative i)
            (_      , a     ) -> VPrefix op a
    BinFieldOp op f e1 e2 -> undefined
    BinEucOp op f e1 e2 -> undefined
{-
    BinOp op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            (VNum i, VNum j) -> VNum (i `f` j)
--            (VPoly i  , VPoly j  ) -> VPoly   (i `f` j)
            (a        , b        ) -> VBinOp  op f a b
--            (VBool i  , VBool j  ) -> VBool   (i `f` j)
-}
    -- e and u are both the same non function type
    BinPred op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            (VNum i, VNum j)              -> VBool (i `f` j)
            (VBool i  , VBool j  )        -> VBool (i `f` j)
            (VLit (LTop _),VLit (LTop _)) -> VBool (f () ())
            (a        , b        )        -> VBinPred op f a b
    BinOpBool op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            (VBool i, VBool j) -> VBool $ i `f` j
            (a      , b      ) -> VBinOpBool op f a b

quoteTerm :: [Name] -> Val -> Tm
quoteTerm ns = \case
    VVar x                      -> Var x
    VApp t u                    -> App (quoteTerm ns t) (quoteTerm ns u)
    VLit l                      -> Lit l
    VLam (freshName ns -> x) t  -> Lam x (quoteTerm (x:ns) (t (VVar x)))
    VPrefix op l                -> Prefix op (quoteTerm ns l)
--    VBinOp op f l r             -> BinOp op f (quoteTerm ns l) (quoteTerm ns r)
    VBinPred op f l r           -> BinPred op f (quoteTerm ns l) (quoteTerm ns r)
    VBinEucOp op f l r          -> BinEucOp op f (quoteTerm ns l) (quoteTerm ns r)
    VBinFieldOp op f l r        -> BinFieldOp op f (quoteTerm ns l) (quoteTerm ns r)

normalForm :: Tm -> State GEnv Tm
normalForm tm = do
    val <- evalTerm [] tm
    env <- get
    lift $ Identity $ quoteTerm (map fst $ getVal env) val