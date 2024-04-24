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
import Control.Monad.Except (throwError)
import Data.Functor.Identity
import Data.Text hiding (map, elem)
import Prelude hiding ((*), (+), negate, (-), quot, rem, lcm, gcd)
import Data.Semiring
--import Data.Euclidean

runTypedTerm :: TTm -> ErrorT GState TTm
runTypedTerm tm = do
    --b <- lift isContextOpen
    env <- get
    let tm' = maybe 
                (maybe 
                    tm 
                    (\x -> perculateZmod (Right x) tm) 
                    (getZmodF env)) 
                (\x -> perculateZmod (Left x) tm) 
                (getZmodN env) in do
        _ <- typeCheck [] tm'
        --return tm'
        normalForm tm'

--- Evaluation ---

freshName :: [Name] -> Name -> Name
freshName ns x = if x `elem` ns
                    then freshName ns $ snoc x '\''
                    else x

perculateZmod :: Either (Complex Frac) (PolyMulti (Complex Frac)) -> TTm -> TTm
perculateZmod f = \case
    TVar n               -> TVar n --tMod (TVar n) (TLit $ LCNum f)
    TApp t u             -> TApp (perculateZmod f t) (perculateZmod f u)
    TLam n t e           -> TLam n t (perculateZmod f e)
    TIfThenElse b t u    -> TIfThenElse (perculateZmod f b) (perculateZmod f t) (perculateZmod f u)
    TLet n e u           -> TLet n (perculateZmod f e) (perculateZmod f u)
    TLit l               -> case l of
                                LCNum _  -> tMod (TLit l) f'
                                LCPoly _ -> tMod (TLit l) f'
                                _        -> TLit l 
    TListCons t u        -> TListCons (perculateZmod f t) (perculateZmod f u)
    TBinPred    op g t u -> TBinPred op g (perculateZmod f t) (perculateZmod f u)
    TBinOpBool  op g t u -> TBinOpBool op g (perculateZmod f t) (perculateZmod f u)
    TBinFieldOp op g t u -> tMod (TBinFieldOp op g (perculateZmod f t) (perculateZmod f u)) f'
    TBinEucOp   op g t u -> tMod (TBinEucOp op g (perculateZmod f t) (perculateZmod f u)) f'
    TBinRingOp  op g t u -> tMod (TBinRingOp op g (perculateZmod f t) (perculateZmod f u)) f'
    TPrefix     op t     -> tMod (TPrefix op (perculateZmod f t)) f'
    where
        f' = either (TLit . LCNum) (TLit . LCPoly) f

vLamApp :: Val -> Val -> ErrorT GState Val
vLamApp (VLam _ _ t) u = t u
vLamApp t          u   = return $ VApp t u

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
        return $ VLam n t (\u -> evalTerm ((n, u):env') e) -- (\u -> evalState (evalTerm ((n, u):env') e) env )
    TIfThenElse b t u -> do
        b' <- evalTerm env' b
        case b' of
            (VBool True ) -> evalTerm env' t
            (VBool False) -> evalTerm env' u
            a             -> return $ VIfThenElse a t u
    TLet n e u -> do
        e' <- evalTerm env' e
        evalTerm ((n, e'):env') u
    TLit l     -> return $ VLit l
    -- We know u is of type list
    TListCons e u -> do --NOT lazy list
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case u' of
            (VLit (LList Nil)) -> VList $ Cons e' Nil
            (VLit (LList l))   -> VList $ Cons e' l
    TPrefix op e -> do
        e' <- evalTerm env' e
        return $ case op of
            Neg    -> case e' of
                        (VCNum  i) -> VCNum  $ negate i
                        (VCPoly i) -> VCPoly $ negate i
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
                                        else throwError $ "Runtime error: (" ++ show op ++ ") can only be called with Poly if it has 1 variable"
            (VCPoly i, VCNum  j) -> if getPolyNumOfVariables i == 1
                                        then return $ VCPoly (i `f` unsafe (complexToComplexPoly j))
                                        else throwError $ "Runtime error: (" ++ show op ++ ") can only be called with Poly if it has 1 variable"
            (VCNum  i, VCPoly j) -> if getPolyNumOfVariables j == 1
                                        then return $ VCPoly (unsafe (complexToComplexPoly i) `f` j)
                                        else throwError $ "Runtime error: (" ++ show op ++ ") can only be called with Poly if it has 1 variable"
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
                                    else throwError "Runtime error: Complex numbers dont have ordering"
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
    VIfThenElse b t u            -> do
        b' <- quoteTerm ns b
        return $ TIfThenElse b' t u
    VLam (freshName ns -> x) t u -> do
        u' <- u $ VVar x
        u'' <- quoteTerm (x:ns) u'
        return $ TLam x t u''
    VPrefix op l                 -> do
        l' <- quoteTerm ns l
        return $ TPrefix op l'
    VBinOpBool op f l r          -> do
        l' <- quoteTerm ns l
        r' <- quoteTerm ns r
        return $ TBinOpBool op f l' r'
    VBinRingOp op f l r          -> do
        l' <- quoteTerm ns l
        r' <- quoteTerm ns r
        return $ TBinRingOp op f l' r'
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

normalForm :: TTm -> ErrorT GState TTm
normalForm tm = do
    val <- evalTerm [] tm
    env <- get
    quoteTerm (map fst $ getVal env) val