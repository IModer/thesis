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
    TLit (LNum  _) -> return TNum
    TLit (LCNum _) -> return TCNum
    TLit (LPoly _) -> return TPoly
    TLit (LCPoly _)-> return TCPoly
    TLit (LBool _) -> return TBool
    TLit (LTop _)  -> return TTop
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
    TIfThenElse b t u -> do
        b' <- typeCheck env b
        t' <- typeCheck env t
        u' <- typeCheck env u
        if b' == TBool
            then
                if  t' == u'
                    then return t'
                    else throwErrorLift $ couldntMatchTypeError t' u' "Both branches of If should have the same return type"
            else throwErrorLift $ couldntMatchTypeError b' TBool "\nCondition to If should have type Bool"
    TLam x t e     -> do
        t' <- typeCheck ((x,t):env) e
        return $ TArr t t'
    TApp e1 e2     -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case t1 of
            (TArr t t') ->  if t == t2
                            then return t'
                            else throwErrorLift ("TypeError :\nCould not match type : " ++ show t ++ "\n\t\twith : " ++ show t2)
            _                     -> throwErrorLift ("TypeError :\nCannot apply type : " ++ show t2 ++ "\n\t       to : " ++ show t1)
    TBinOpBool op _ e1 e2 -> bothConformTo TBool (e1,e2) op env
    TBinPred op f e1 e2 -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        if t1 == t2 && hasOrd t1
            then return TBool
            else throwErrorLift ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show t1 ++ " and " ++ show t2)
    -- This should allow Poly `op` Number and Number `op` Poly
    TBinFieldOp op f e1 e2 -> undefined
    TBinEucOp op f e1 e2 -> undefined
    --TBinOp op f e1 e2 -> bothTypesEqual (e1, e2) isNumType op env
{-
    TBinOp op f e1 e2 ->
        case op of
            Eq  -> bothTypesEqual        (e1, e2) Eq  env
            And -> bothConformTo TBool   (e1, e2) And env
            Or  -> bothConformTo TBool   (e1, e2) Or  env
            _   -> bothConformTo TNumber (e1,e2)  op  env
-}
    TPrefix op e    -> do
        e' <- typeCheck env e
        case (op, e') of
            (Neg    , TNum)    -> return TNum
            (Factor , TPoly)   -> return TPoly
            (Der    , TPoly)   -> return TPoly
            (Irred  , TPoly)   -> return TBool
            (_      , _      ) -> throwErrorLift ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show e')

couldntMatchTypeError :: Type -> Type -> String -> String
couldntMatchTypeError t1 t2 reason = "TypeError : \nCould not match type : " 
    ++ show t1 ++ "\n\t\twith : "
    ++ show t2 ++ "\nReason: " ++ reason

-- Maybe inline this again
bothTypesEqual :: (TTm,TTm) -> (Type -> Bool) ->  BinOp -> TEnv -> ErrorT GState Type 
bothTypesEqual (e1,e2) pred op env  = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    if t1 == t2 && pred t1 then
        return t1
    else
        throwErrorLift ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show t1 ++ " and " ++ show t2)

hasOrd :: Type -> Bool
hasOrd = flip elem [TNum, TBool]

{-
-}
isNumType :: Type -> Bool
isNumType e = e `elem` [TNum, TCNum, TPoly, TCPoly]

notFunctionType :: Type -> Bool
notFunctionType e | e `elem` [TBool, TNum, TCNum, TPoly, TCPoly, TTop] = True
                  | otherwise = False

bothConformTo :: Type -> (TTm,TTm) -> BinOp -> TEnv -> ErrorT GState Type
bothConformTo t (e1,e2) op env  = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    if t1 == t && t2 == t then
        return t
    else
        throwErrorLift ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show t1 ++ " and " ++ show t2)