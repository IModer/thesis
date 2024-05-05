--{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Core.Interpreter(runTypedTerm, evalTerm) where

import Core.AST
import Core.TypeChecker
import Core.Classes
import Core.Types

import Control.Monad.Fix (mfix)
import Control.Monad.State.Class (get)
import Control.Monad.Except (throwError)
import Data.Text (snoc)
import Prelude hiding (negate)
import Data.Semiring (negate, zero)

-- Typechecks and calculates the normalform of TTm
-- If there is a context open then first calls perculateZmod on it
runTypedTerm :: TTm -> ErrorT GState TTm
runTypedTerm tm = do
    env <- get
    let tm' = maybe
                (maybe
                    tm 
                    (\x -> perculateZmod (Right x) tm)
                    (getZmodF env))
                (\x -> perculateZmod (Left x) tm)
                (getZmodN env) in do
        _ <- typeCheck [] tm'
        normalForm tm'

freshName :: [Name] -> Name -> Name
freshName ns x = if x `elem` ns
                    then freshName ns $ snoc x '\''
                    else x

-- Inserts (mod f) into the AST in the appopriate places
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
    TFix n               -> TFix n
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

-- 
evalTerm :: VEnv -> TTm -> ErrorT GState Val
evalTerm env' = \case
    TVar n     -> do
        env <- get
        maybe   (return $ unsafe $ lookup n $ getVal env)
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
    TFix m         -> do
        m' <- evalTerm env' m
        -- because of typechecking we know m : t ~> t'
        -- so its a VLam
        case m' of
            (VLam _ _ e) -> mfix e
            _            -> error "unreachable : TFix m, m should have be a (VLam x t e)"
    TListCons e u -> do
        -- not a lazy list, we force eval e and u
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        -- because of typechecking we know u' : List
        -- so its a VList
        return $ case u' of
            (VList []) -> VList $ e' : []
            (VList l)   -> VList $ (e' : l)
            _            -> error "unreachable : TListCons e u, u should have be a (VList l)"
    TPrefix op e -> do
        e' <- evalTerm env' e
        return $ case op of
            Neg    -> case e' of
                        (VCNum  i) -> VCNum  $ negate i
                        (VCPoly i) -> VCPoly $ negate i
                        (a       ) -> VPrefix op a
    TBinFieldOp op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        -- because of typechecking we know e1,e2 : CNum
        -- if they are not a literal means they are stuck
        -- or they are incorrect but we do not explicitly cover those cases
        return $ case (e', u') of 
            (VCNum i, VCNum j) -> VCNum (i `f` j)
            (a      ,       b) -> VBinFieldOp op f a b
    TBinEucOp op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        -- e' and u' are in [TCNum, TCPoly]
        -- if they are not literals we have the same case as TBinFieldOp
        -- because the euclidian ops : div and mod only work with monopoly
        -- we also check if the polinomials at max have 1 variable
        case (e', u') of
            (VCPoly i, VCPoly j) -> let (i', j') = (getPolyNumOfVariables i , getPolyNumOfVariables j) in
                                        if (i' == 1 || i' == 0) && (j' == 1 || j' == 0)
                                            then return $ VCPoly (i `f` j)
                                            else throwError $ "Runtime error: (" ++ show op ++ ") can only be called with Poly if it has 1 variable"
            -- We lift VCNum into a poly
            (VCPoly i, VCNum  j) -> let i' = getPolyNumOfVariables i in
                                        if (i' == 1 || i' == 0)
                                            then return $ VCPoly (i `f` unsafe (complexToComplexPoly j))
                                            else throwError $ "Runtime error: (" ++ show op ++ ") can only be called with Poly if it has 1 variable"
            (VCNum  i, VCPoly j) -> let j' = getPolyNumOfVariables j in
                                        if (j' == 1 || j' == 0)
                                            then return $ VCPoly (unsafe (complexToComplexPoly i) `f` j)
                                            else throwError $ "Runtime error: (" ++ show op ++ ") can only be called with Poly if it has 1 variable"
            (VCNum  i, VCNum  j) -> return $ VCNum  (i `f` j)
            (a       , b       ) -> return $ VBinEucOp op f a b
    TBinRingOp op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        return $ case (e', u') of
            -- e' and u' are in [TCNum, TCPoly]
            -- if they are not literals we have the same case as TBinEucOp
            (VCPoly i, VCPoly j) -> VCPoly (i `f` j)
            (VCPoly i, VCNum  j) -> VCPoly (i `f` unsafe (complexToComplexPoly j))
            (VCNum  i, VCPoly j) -> VCPoly (unsafe (complexToComplexPoly i) `f` j)
            (VCNum  i, VCNum  j) -> VCNum  (i `f` j)
            (a       , b       ) -> VBinRingOp op f a b
    TBinPred op f e u -> do
        e' <- evalTerm env' e
        u' <- evalTerm env' u
        -- we have eq for all types but ord only for [Bool, Num, Top]
        if op == Eq
            then case (e', u') of
                (VCPoly i, VCPoly j) -> return $ VBool (i == j)
                (VCNum i , VCNum j ) -> return $ VBool (i == j)
                (VBool i , VBool j ) -> return $ VBool (i == j)
                (VTop    , VTop    ) -> return $ VBool True
                (a       , b       ) -> return $ VBinPred op f a b            
            else case (e', u') of
                (VCNum i , VCNum j) -> if imag i == zero && imag j == zero 
                                        then return $ VBool (i `f` j)
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

-- Turns Val into TTm with var names from [Name] and fresh names for lam-s
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