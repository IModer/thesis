{-# LANGUAGE LambdaCase #-}

module Core.TypeChecker where

import Control.Monad.Except (liftEither, throwError)
import Control.Monad.State (lift, get)

import Core.AST
import Lib
import Core.Classes
import qualified Data.Text as T
import Data.List (intercalate)

typeCheck :: TEnv -> TTm -> ErrorT GState Type
typeCheck env = \case
{-
    TLit (LNum  _) -> return TNum
    TLit (LPoly _) -> return TPoly
-}
    TLit (LCNum _) -> return TCNum
    TLit (LCPoly _)-> return TCPoly
    TLit (LBool _) -> return TBool
    TLit (LTop _)  -> return TTop
    TLit (LList _) -> return TList
    TListCons e u  -> do
        t1 <- typeCheck env e
        t2 <- typeCheck env u
        if t2 == TList 
            then return TList
            else throwError $ "TypeError :\n(::) cannot be called with : " ++ show t1 ++ " and " ++ show t2
    TVar x         -> do
        env' <- get
        case lookup x env of
            Just a  -> return a
            Nothing -> case lookup x $ getType env' of
                    Just b  -> return b
                    Nothing -> throwError
                        ("Cannot find variable : " ++ T.unpack x)
    TLet x e u     -> do
        t <- typeCheck env e
        typeCheck ((x,t):env) u
    TIfThenElse b t u -> do
        b' <- typeCheck env b
        t' <- typeCheck env t
        u' <- typeCheck env u
        if b' == TBool
            then
                if  t' == u'
                    then return t'
                    else throwError $ couldntMatchTypeError t' u' "Both branches of If should have the same return type"
            else throwError $ couldntMatchTypeError b' TBool "\nCondition to If should have type Bool"
    TLam x t e     -> do
        t' <- typeCheck ((x,t):env) e
        return $ t ~> t'
    TApp e1 e2     -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case t1 of
            (TArr t t') ->  if t == t2
                            then return t'
                            else throwError ("TypeError :\nCould not match type : " ++ show t ++ "\n\t\twith : " ++ show t2)
            _                     -> throwError ("TypeError :\nCannot apply type : " ++ show t2 ++ "\n\t       to : " ++ show t1)
    TBinOpBool op _ e1 e2 -> bothConformTo TBool (e1,e2) op env
    TBinPred op f e1 e2 -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        if t1 == t2 && hasOrd t1
            then return TBool
            else throwError ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show t1 ++ " and " ++ show t2)
    TBinFieldOp op f e1 e2 -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case (t1, t2) of
            (TCNum , TCNum) -> return TCNum
            (a     , b    ) -> throwError $ cannotBeCalledWithError t1 t2 op
    TBinRingOp op f e1 e2 -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        if hasEuclid t1 && hasEuclid t2
            then case (t1, t2) of
                (TCPoly , _     ) -> return TCPoly
                (_      , TCPoly) -> return TCPoly
                (TCNum  , TCNum ) -> return TCNum
            else throwError $ cannotBeCalledWithError t1 t2 op
    -- az op csak Div, Mod
    TBinEucOp op f e1 e2 -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        if hasEuclid t1 && hasEuclid t2
            then case (t1, t2) of
                (TCPoly , _     ) -> return TCPoly
                (_      , TCPoly) -> return TCPoly
                (TCNum  , TCNum ) -> return TCNum
            else throwError $ cannotBeCalledWithError t1 t2 op
    -- It might be worth it to abstract out Poly -> Poly ops
    TPrefix op e    -> do
        e' <- typeCheck env e
        case op of
            Neg    -> if hasEuclid e' 
                        then return e' 
                        else throwError $ cannotBeCalledWithError' e' op

cannotBeCalledWithError :: Type -> Type -> BinOp -> String
cannotBeCalledWithError t1 t2 op = "TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show t1 ++ " and " ++ show t2

cannotBeCalledWithError' :: Type -> PrefixOp -> String
cannotBeCalledWithError' ts op = "TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show ts 

couldntMatchTypeError :: Type -> Type -> String -> String
couldntMatchTypeError t1 t2 reason = "TypeError : \nCould not match type : " 
    ++ show t1 ++ "\n\t\twith : "
    ++ show t2 ++ "\nReason: " ++ reason

isPoly :: Type -> Bool
isPoly = (== TCPoly)

hasRing :: Type -> Bool
hasRing = flip elem [TCNum, TCPoly]

hasEuclid :: Type -> Bool
hasEuclid = flip elem [TCNum, TCPoly]

hasOrd :: Type -> Bool
hasOrd = flip elem [TCNum, TBool, TTop]

bothConformTo :: Type -> (TTm,TTm) -> BinOp -> TEnv -> ErrorT GState Type
bothConformTo t (e1,e2) op env  = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    if t1 == t && t2 == t then
        return t
    else
        throwError ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show t1 ++ " and " ++ show t2)