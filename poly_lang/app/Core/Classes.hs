module Core.Classes where

import Control.Monad.State
import Control.Monad.Except
import Core.AST

-- Error type for typechecking errors
-- is a monad

type ErrorT = ExceptT String

-- Custome state for type context and value context
-- As long as the getter and setter functions are used 
-- this is can be considered as abstract
type TEnv = [(Name, Type)]
type VEnv  = [(Name, Val)]

insertType :: (Name, Type) -> GEnv -> GEnv
insertType xt (GEnv tenv env) = GEnv (xt : tenv) env

insertVal :: (Name, Val) -> GEnv -> GEnv
insertVal nv (GEnv tenv env) = GEnv tenv (nv : env)

getType :: GEnv -> TEnv
getType = typeEnv

getVal :: GEnv -> VEnv
getVal = nameEnv

data GEnv = GEnv {typeEnv :: TEnv , 
                  nameEnv :: VEnv}

type GState = State GEnv
