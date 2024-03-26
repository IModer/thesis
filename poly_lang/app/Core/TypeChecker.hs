{-# LANGUAGE LambdaCase #-}

module Core.TypeChecker where

import Control.Monad.Except (liftEither)
import Control.Monad.State (lift, get)

import Core.AST
import Lib
import Core.Classes
import qualified Data.Text as T

typeCheck :: TEnv -> TTm -> ErrorT GState Type
typeCheck env = \case
    TLit (LNumber  _) -> return TNumber
    TLit (LBool _) -> return TBool
    TLit LTop      -> return TTop
    TVar x         -> do
        env' <- get
        case lookup x $ getType env' of
            Just a  -> return a
            Nothing -> case lookup x env of
                    Just b  -> return b
                    Nothing -> throwErrorLift 
                        ("Cannot find variable : " ++ T.unpack x)
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
                                else throwErrorLift ("TypeError :\nCould not match type : " ++ show t ++ "\n\t\twith : " ++ show t2)
            _                     -> throwErrorLift ("TypeError :\nCannot apply type : " ++ show t2 ++ "\n\t       to : " ++ show t1)
    TBinOpBool op _ e1 e2 -> bothConformTo TBool   (e1,e2) op env
    TBinOpNum  op _ e1 e2 -> bothConformTo TNumber (e1,e2) op env 
    TBinOp op e1 e2 -> 
        case op of
            Eq  -> bothTypesEqual      (e1, e2) Eq env
            And -> bothConformTo TBool (e1, e2) And env
            Or  -> bothConformTo TBool (e1, e2) Or env
            _   -> bothConformTo TNumber (e1,e2) op env
    TPrefix op e    -> do
        e' <- typeCheck env e
        case (op, e') of
            (Neg    , TNumber) -> return TNumber
            (Factor , TNumber) -> return TNumber
            (Der    , TNumber) -> return TNumber
            (Irred  , TNumber) -> return TBool
            (_      , _      ) -> throwErrorLift ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show e')

bothTypesEqual :: (TTm,TTm) -> BinOp -> TEnv -> ErrorT GState Type 
bothTypesEqual (e1,e2) op env  = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    if t1 == t2 then
        return t1
    else
        throwErrorLift ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show t1 ++ " and " ++ show t2)

bothConformTo :: Type -> (TTm,TTm) -> BinOp -> TEnv -> ErrorT GState Type
bothConformTo t (e1,e2) op env  = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    if t1 == t && t2 == t then
        return t
    else
        throwErrorLift ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show t1 ++ " and " ++ show t2)