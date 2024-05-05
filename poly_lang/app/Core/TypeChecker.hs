{-# LANGUAGE LambdaCase #-}

module Core.TypeChecker(typeCheck) where

import Control.Monad.Except (throwError)
import Control.Monad.State (get)

import Core.AST
import Core.Classes
import qualified Data.Text as T
import Data.List()

-- We try to typecheck a TTm
-- TEnv is the local env for vars in let and lam
-- We return into ErrorT GState Type :
--      throwError if we could not come up with a type
-- We dont modify GState
typeCheck :: TEnv -> TTm -> ErrorT GState Type
typeCheck env = \case
    -- Checking literals is easy, we just synthesis the type
    TLit (LCNum _) -> return TCNum
    TLit (LCPoly _)-> return TCPoly
    TLit (LBool _) -> return TBool
    TLit (LTop _)  -> return TTop
    TLit (LList _) -> return TList
    -- a = fix e
    -- e : t1
    -- if t1 == t -> t'
    --     then a : t
    --     else error
    TFix e         -> do
        t1 <- typeCheck env e
        case t1 of
            (TArr t t') | t == t' -> return t
            _                     -> throwError "TypeError : Argument to Fix should have type : a -> a, where a is any type"
    -- a = e :: u
    -- e : t1, u : t2
    -- if we have t2 == List 
    --     then a : List
    --     else error                   
    TListCons e u  -> do
        t1 <- typeCheck env e
        t2 <- typeCheck env u
        if t2 == TList 
            then return TList
            else throwError $ "TypeError :\n(::) cannot be called with : " ++ show t1 ++ " and " ++ show t2
    -- We check x in both the local env (env) and the global
    -- from GState (env')
    TVar x         -> do
        env' <- get
        case lookup x env of
            Just a  -> return a
            Nothing -> case lookup x $ getType env' of
                    Just b  -> return b
                    Nothing -> throwError
                        ("Cannot find variable : " ++ T.unpack x)
    -- e : t
    -- we try to typecheck u given (x : t) 
    TLet x e u     -> do
        t <- typeCheck env e
        typeCheck ((x,t):env) u
    -- a = if b then t else u
    -- b : b', t : t', u : u'
    -- if b : Bool & u' == t'
    --     then a : t'
    --     else error
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
    -- a = \x : t . e
    -- we try to typecheck e given (x : t)
    -- if we dont fail then a : t ~> t' 
    TLam x t e     -> do
        t' <- typeCheck ((x,t):env) e
        return $ t ~> t'
    -- a = e1 e2
    -- e1 : t1, e2 : t2
    -- if t1 == t ~> t' & t == t2
    --     then a : t'
    --     else error
    TApp e1 e2     -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case t1 of
            (TArr t t') ->  if t == t2
                            then return t'
                            else throwError ("TypeError :\nCould not match type : " ++ show t ++ "\n\t\twith : " ++ show t2)
            _                     -> throwError ("TypeError :\nCannot apply type : " ++ show t2 ++ "\n\t       to : " ++ show t1)
    TBinOpBool op _ e1 e2 -> bothConformTo TBool (e1,e2) op env
    TBinPred op _ e1 e2 -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        if (op == Eq && t1 == t2 && hasEq t1) || (op /= Eq && t1 == t2 && hasOrd t1)
            then return TBool
            else throwError ("TypeError :\n" ++ ("(" ++ show op ++ ")") ++ " cannot be called with : " ++ show t1 ++ " and " ++ show t2)
    TBinFieldOp op _ e1 e2 -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        case (t1, t2) of
            (TCNum , TCNum) -> return TCNum
            (_     , _    ) -> throwError $ cannotBeCalledWithError t1 t2 op
    TBinRingOp op _ e1 e2 -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        if hasEuclid t1 && hasEuclid t2
            -- hasEuclid x == x `elem` [TCPoly,TCNum]
            -- this means these cases cover all
            then case (t1, t2) of
                (TCPoly , _     ) -> return TCPoly
                (_      , TCPoly) -> return TCPoly
                (TCNum  , TCNum ) -> return TCNum
                _                 -> error "unreachable"
            else throwError $ cannotBeCalledWithError t1 t2 op
    -- az op csak Div, Mod
    TBinEucOp op _ e1 e2 -> do
        t1 <- typeCheck env e1
        t2 <- typeCheck env e2
        if hasEuclid t1 && hasEuclid t2
        -- hasEuclid x == x `elem` [TCPoly,TCNum]
        -- this means these cases cover all
            then case (t1, t2) of
                (TCPoly , _     ) -> return TCPoly
                (_      , TCPoly) -> return TCPoly
                (TCNum  , TCNum ) -> return TCNum
                _                 -> error "unreachable"
            else throwError $ cannotBeCalledWithError t1 t2 op
    TPrefix op e    -> do
        e' <- typeCheck env e
        case op of
            Neg    -> if hasEuclid e'
                        then return e'
                        else throwError $ cannotBeCalledWithError' e' op

cannotBeCalledWithError :: Type -> Type -> BinOp -> String
cannotBeCalledWithError t1 t2 op = "TypeError :\n" ++ 
    ("(" ++ show op ++ ")") ++ 
    " cannot be called with : " ++ 
    show t1 ++ 
    " and " ++ 
    show t2

cannotBeCalledWithError' :: Type -> PrefixOp -> String
cannotBeCalledWithError' ts op = "TypeError :\n" ++
    ("(" ++ show op ++ ")") ++
    " cannot be called with : "++
    show ts 

couldntMatchTypeError :: Type -> Type -> String -> String
couldntMatchTypeError t1 t2 reason = "TypeError : \nCould not match type : " ++ 
    show t1 ++
    "\n\t\twith : " ++
    show t2 ++
    "\nReason: " ++
    reason

hasEuclid :: Type -> Bool
hasEuclid = flip elem [TCNum, TCPoly]

hasEq :: Type -> Bool
hasEq = flip elem [TCPoly, TCNum, TBool, TTop]

hasOrd :: Type -> Bool
hasOrd = flip elem [TCNum, TBool, TTop]

bothConformTo :: Type -> (TTm,TTm) -> BinOp -> TEnv -> ErrorT GState Type
bothConformTo t (e1,e2) op env  = do
    t1 <- typeCheck env e1
    t2 <- typeCheck env e2
    if t1 == t && t2 == t then
        return t
    else
        throwError $ cannotBeCalledWithError t1 t2 op