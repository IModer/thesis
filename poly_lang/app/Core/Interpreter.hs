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
        return $ case op of
            Neg    -> case e' of
                        (VNum   i) -> VNum   $ negate i
                        (VCNum  i) -> VCNum  $ negate i
                        (VPoly  i) -> VPoly  $ negate i
                        (VCPoly i) -> VCPoly $ negate i
                        (a       ) -> VPrefix op a
            Factor -> case e' of
                        (VPoly  i) -> VPoly  $ factor i
                        (VCPoly i) -> VCPoly $ factor i
                        (a       ) -> VPrefix op a
            Der    -> case e' of
                        (VPoly  i) -> VPoly  $ derivative i
                        (VCPoly i) -> VCPoly $ derivative i
                        (a       ) -> VPrefix op a
            Irred  -> case e' of
                        (VPoly  i) -> VPoly  $ irred i
                        (VCPoly i) -> VCPoly $ irred i
                        (a       ) -> VPrefix op a
    -- e1 and e2 are in [CNum, Num]
    BinFieldOp op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of 
            (VCNum i, VCNum j) -> VCNum (i `f` j)
            (VNum  i, VCNum j) -> VCNum (fracToComplex i `f` j)
            (VCNum i, VNum  j) -> VCNum (i `f` fracToComplex j)
            (VNum  i, VNum  j) -> VNum  (i `f` j)
            (a      ,       b) -> VBinFieldOp op f a b
    -- e1 and e1 are in [TNum, TCNum, TPoly, TCPoly]
    -- with the exception that (e1,e2) cannot be (TPoly, TCNum) or (TCNum, TPoly)
    BinEucOp op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            (VCPoly i, VCPoly j) -> VCPoly (i `f` j)
            (VPoly  i, VPoly  j) -> VPoly  (i `f` j)
            (VCNum  i, VCNum  j) -> VCNum  (i `f` j)
            (VNum   i, VNum   j) -> VNum   (i `f` j)
            -- Poly & CNum -> CPoly
            (VPoly i, VCNum  j ) -> VCPoly (polyToCPoly i `f` unsafe (complexToComplexPoly j))
            (VCNum i, VPoly  j ) -> VCPoly (unsafe (complexToComplexPoly i) `f` polyToCPoly j)
            -- CPoly & Poly -> CPoly
            (VCPoly i, VPoly  j) -> VCPoly (i `f` polyToCPoly j)
            (VPoly  i, VCPoly j) -> VCPoly (polyToCPoly i `f` j)
            -- CPoly & CNum -> CPoly
            (VCPoly i, VCNum  j) -> VCPoly (i `f` unsafe (complexToComplexPoly j))
            (VCNum  i, VCPoly j) -> VCPoly (unsafe (complexToComplexPoly i) `f` j)
            -- CPoly & Num -> CPoly
            (VCPoly i, VNum   j) -> VCPoly (i `f` unsafe (fracToComplexPoly j))
            (VNum   i, VCPoly j) -> VCPoly (unsafe (fracToComplexPoly i) `f` j)
            -- Poly & Num -> Poly
            (VPoly  i, VNum   j) -> VPoly  (i `f` unsafe (fracToPoly j))
            (VNum   i, VPoly  j) -> VPoly  (unsafe (fracToPoly i) `f` j)
            -- CNum & Num -> CNum
            (VNum   i, VCNum  j) -> VCNum  (fracToComplex i `f` j)
            (VCNum  i, VNum   j) -> VCNum  (i `f` fracToComplex j)
            (a       , b       ) -> VBinEucOp op f a b
    -- e and u are both the same and have Ord [Bool, Num, Top]
    BinPred op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            (VNum i  , VNum j ) -> VBool (i `f` j)
            (VBool i , VBool j) -> VBool (i `f` j)
            (VTop    , VTop   ) -> VBool True
            (a       , b      ) -> VBinPred op f a b
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
    VBinPred op f l r           -> BinPred op f (quoteTerm ns l) (quoteTerm ns r)
    VBinEucOp op f l r          -> BinEucOp op f (quoteTerm ns l) (quoteTerm ns r)
    VBinFieldOp op f l r        -> BinFieldOp op f (quoteTerm ns l) (quoteTerm ns r)

normalForm :: Tm -> State GEnv Tm
normalForm tm = do
    val <- evalTerm [] tm
    env <- get
    lift $ Identity $ quoteTerm (map fst $ getVal env) val