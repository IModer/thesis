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
import Control.Monad.Except (runExceptT, throwError)
import Data.Functor.Identity
import Data.Text hiding (map, elem)
import Prelude hiding ((*), (+), negate, (-), quot, rem, lcm, gcd)
import Data.Semiring
import Data.Euclidean

runTypedTerm :: TTm -> ErrorT GState TTm
runTypedTerm tm = do
    _ <- typeCheck [] tm
    normalForm tm

--- Evaluation ---

freshName :: [Name] -> Name -> Name
freshName ns x = if x `elem` ns
                    then freshName ns $ snoc x '\''
                    else x

vLamApp :: Val -> Val -> ErrorT GState Val
vLamApp (VLam _ _ t) u = t u
vLamApp t          u = return $ VApp t u

--evalTerm :: VEnv -> TTm -> State GEnv Val
evalTerm :: VEnv -> TTm -> ErrorT GState Val
evalTerm env' = \case
    TVar n     -> do
        env <- get
        maybe   (return $ fromJust $ lookup n $ getVal env)
                return
                (lookup n env')
    TApp t u   -> do
        t' <- evalTerm env' t
        u' <- evalTerm env' u
        vLamApp t' u'
    TLam n t e -> do
        env <- get
        return $ VLam n t (\u -> evalTerm ((n, u):env') e) -- (\u -> evalState (evalTerm ((n, u):env') e) env )
    TIfThenElse b t u -> do
        b' <- evalTerm env' b
        case b' of
            (VBool True ) -> evalTerm env' t
            (VBool False) -> evalTerm env' u
            _             -> undefined
    TLet n e u -> do
        e' <- evalTerm env' e
        evalTerm ((n, e'):env') u
    TLit l     -> return $ VLit l
    TPrefix op e -> do
        e' <- evalTerm env' e
        return $ case op of
            Neg    -> case e' of
                        (VCNum  i) -> VCNum  $ negate i
                        (VCPoly i) -> VCPoly $ negate i
                        (a       ) -> VPrefix op a
            Factor -> case e' of
                        (VCPoly i) -> VCPoly $ factor i
                        (a       ) -> VPrefix op a
            Der    -> case e' of
                        (VCPoly i) -> VCPoly $ derivative i
                        (a       ) -> VPrefix op a
            Irred  -> case e' of
                        (VCPoly i) -> VCPoly $ irred i
                        (a       ) -> VPrefix op a
    -- e1 and e2 are CNum
    TBinFieldOp op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of 
            (VCNum i, VCNum j) -> VCNum (i `f` j)
            (a      ,       b) -> VBinFieldOp op f a b
    -- e1 and e1 are in [TCNum, TCPoly]
    TBinEucOp op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        case (e', u') of
            (VCPoly i, VCPoly j) -> if getPolyNumOfVariables i == 1 && getPolyNumOfVariables j == 1
                                        then return $ VCPoly (i `f` j)
                                        else throwErrorLift $ "Runtime error: (" ++ show op ++ ") can only be called with Poly if it has 1 variable"
            (VCPoly i, VCNum  j) -> if getPolyNumOfVariables i == 1
                                        then return $ VCPoly (i `f` unsafe (complexToComplexPoly j))
                                        else throwErrorLift $ "Runtime error: (" ++ show op ++ ") can only be called with Poly if it has 1 variable"
            (VCNum  i, VCPoly j) -> if getPolyNumOfVariables j == 1
                                        then return $ VCPoly (unsafe (complexToComplexPoly i) `f` j)
                                        else throwErrorLift $ "Runtime error: (" ++ show op ++ ") can only be called with Poly if it has 1 variable"
            (VCNum  i, VCNum  j) -> return $ VCNum  (i `f` j)
            (a       , b       ) -> return $ VBinEucOp op f a b
    TBinRingOp op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            (VCPoly i, VCPoly j) -> VCPoly (i `f` j)
            (VCPoly i, VCNum  j) -> VCPoly (i `f` unsafe (complexToComplexPoly j))
            (VCNum  i, VCPoly j) -> VCPoly (unsafe (complexToComplexPoly i) `f` j)
            (VCNum  i, VCNum  j) -> VCNum  (i `f` j)
            (a       , b       ) -> VBinRingOp op f a b
    -- e and u are both the same and have Ord [Bool, Num, Top]
    TBinPred op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        case (e', u') of
            (VCNum i , VCNum j) -> if imag i == zero && imag j == zero then return $ VBool (i `f` j)
                                    else throwErrorLift "Runtime error: Complex numbers dont have ordering"
            (VBool i , VBool j) -> return $ VBool (i `f` j)
            (VTop    , VTop   ) -> return $ VBool True
            (a       , b      ) -> return $ VBinPred op f a b
    TBinOpBool op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            (VBool i, VBool j) -> VBool $ i `f` j
            (a      , b      ) -> VBinOpBool op f a b

--quoteTerm :: [Name] -> Val -> TTm
quoteTerm :: [Name] -> Val -> ErrorT GState TTm
quoteTerm ns = \case
    VVar x                       -> return $ TVar x
    VApp t u                     -> do
        t' <- quoteTerm ns t
        u' <- quoteTerm ns u
        return $ TApp t' u'
    VLit l                       -> return $ TLit l
    VLam (freshName ns -> x) t u -> do
        u' <- u $ VVar x
        u'' <- quoteTerm (x:ns) u'
        return $ TLam x t u''
    VPrefix op l                 -> do
        l' <- quoteTerm ns l
        return $ TPrefix op l'
    VBinPred op f l r            -> do
        l' <- quoteTerm ns l
        r' <- quoteTerm ns r
        return $ TBinPred op f l' r'
    VBinEucOp op f l r           -> do
        l' <- quoteTerm ns l
        r' <- quoteTerm ns r
        return $ TBinEucOp op f l' r'
    VBinFieldOp op f l r         -> do
        l' <- quoteTerm ns l
        r' <- quoteTerm ns r
        return $ TBinFieldOp op f l' r'


--normalForm :: TTm -> State GEnv TTm
normalForm :: TTm -> ErrorT GState TTm
normalForm tm = do
    val <- evalTerm [] tm
    env <- get
    quoteTerm (map fst $ getVal env) val

-- TTm -> ErrorT GState TTm