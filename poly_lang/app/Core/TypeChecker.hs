{-# LANGUAGE LambdaCase #-}

module Core.TypeChecker where

import Control.Monad.State (StateT, lift, get)
import Core.AST
import Lib
import Core.Classes
import qualified Data.Text as T

typeCheck :: TEnv -> TTm -> StateT GEnv Error Type
typeCheck env = \case
    TLit (LNumber  _) -> return TNumber
    TLit (LBool _) -> return TBool
    TLit LTop      -> return TTop
    TVar x         -> do
        env' <- get
        maybe   (maybe (lift $ Error ("Cannot find variable : " ++ T.unpack x))
                (return) 
                (lookup x env))
            (return)
            (lookup x (getType env'))
    TLet x e u     -> do
        t <- typeCheck env e
        typeCheck ((x,t):env) u
    TLam x t e     -> do 
        t' <- typeCheck ((x,t):env) e
        return $ TArr t t'
    TApp e1 e2     -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case t1 of
            (TArr t t') -> if t == t2
                                then return t'
                                else lift $ Error ("TypeError :\nCould not match type : " ++ show t ++ "\n\t\twith : " ++ show t2)
            _                     -> lift $ Error ("TypeError :\nCannot apply type : " ++ show t2 ++ "\n\t       to : " ++ show t1)
    TBinOpBool op _ e1 e2 -> bothTypesEqual op env e1 e2 TBool
    TBinOpNum  op _ e1 e2 -> bothTypesEqual op env e1 e2 TNumber
    TBinOp op e1 e2 -> if op `elem` [And, Or]
        then bothTypesEqual op env e1 e2 TBool
        else bothTypesEqual op env e1 e2 TNumber
    TPrefix op e    -> do
        e' <- typeCheck env e
        case (op, e') of
            (Neg    , TNumber) -> return TNumber
            (Factor , TNumber) -> return TNumber
            (Der    , TNumber) -> return TNumber
            (Irred  , TNumber) -> return TBool
            (_      , _      ) -> lift $ Error ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show e')

bothTypesEqual :: BinOp -> TEnv -> TTm -> TTm -> Type -> StateT GEnv Error Type
bothTypesEqual op env e1 e2 t  = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    if t1 == t && t2 == t then
        return t
    else
        lift $ Error ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show t1 ++ " and " ++ show t2)