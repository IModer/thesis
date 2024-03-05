{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE LambdaCase, ViewPatterns #-}
module Core where

import Data.Maybe
import Control.Monad.Trans       --lift
import Control.Monad.State.Class --MonadClass
import Control.Monad.State.Lazy  --StateT


type Name = String

type TEnv = [(Name, Type)]
type Env  = [(Name,TTm)]

data SEnv = SEnv {typeEnv :: TEnv , 
                  nameEnv :: Env}


--eval :: SEnv -> String -> (SEnv, String)
--eval :: MonadState SEnv m => String -> m String
eval :: String -> State SEnv String --which is :: String -> StateT SEnv Ident String 
eval cs = case parseString cs of
    Left a   -> return "Parse error"  --TODO : prettyPrint
    Right tm -> helper (runTypedTerm' tm) -- REALLY TODO :: StateT SEnv Maybe String -> State SEnv String
        -- This could be return $ maybe "Type error" prettyPrint tm'
        
        --return $ maybe "Type error" prettyPrint (runTypedTerm' tm)
        
        --return $ case tm' of
        --    Just tm'' -> prettyPrint tm''
        --    Nothing   -> "Type error"
        where 
            helper :: StateT SEnv Maybe Tm -> State SEnv String
            helper st = do
                env <- get
                mapStateT (\case 
                    (Just (tm',env')) -> return (prettyPrint tm',env')
                    Nothing -> return $ ("Type error",env) ) st

--runTypedTerm' :: SEnv -> TTm -> (SEnv, Maybe Tm)
-- TTm -> State SEnv (Maybe Tm)
-- Ha failelt a typeCheck akkor nem kéne az új envet tovább adni
-- Amúgy StateT Maybe SEnv Tm

--runTypedTerm' :: MonadState SEnv m => TTm -> m (Maybe Tm)
runTypedTerm' :: TTm -> StateT SEnv Maybe Tm
runTypedTerm' tm = do
    t <- typeCheck' tm
    (state . runState) $ normalForm' (loseType tm)

-- TTm -> Maybe Type
-- SEnv -> TTm -> (SEnv, Maybe Type)
-- TTm -> State SEnv (Maybe Type)
--typeCheck' :: MonadState SEnv m => TTm -> m (Maybe Type)
typeCheck' :: TTm -> StateT SEnv Maybe Type
typeCheck' = \case
    TLit (LInt  _) -> return TInt
    TLit (LBool _) -> return TBool
    TLit LTop      -> return TTop
    TVar x         -> undefined
    TLet x e u     -> do
        t <- typeCheck' e
        modify (\(SEnv tenv env) -> SEnv ((x,t) : tenv) env)
        typeCheck' u
    TLam x t e     -> undefined
    TApp e1 e2     -> undefined
    TPlus e1 e2    -> undefined
    TTimes e1 e2   -> undefined
    TAnd e1 e2     -> undefined
    TOr e1 e2      -> undefined

--normalForm' :: SEnv -> Tm -> (SEnv, Tm)
normalForm' :: Tm -> State SEnv Tm
--normalForm' :: MonadState SEnv m => Tm -> m Tm
normalForm' = undefined

--quoteTerm' :: SEnv -> Val -> (SEnv, Tm) 
--quoteTerm' :: Val -> State SEnv Tm
quoteTerm' :: MonadState SEnv m => Val -> m Tm
quoteTerm' = undefined

--evalTerm' :: Env -> Tm -> (Env, Val)
evalTerm' :: MonadState SEnv m => Tm -> m Val
evalTerm' = undefined