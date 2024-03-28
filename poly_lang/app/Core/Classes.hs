module Core.Classes where

import Control.Monad.State
import Control.Monad.Except
import Core.AST
import Ring
import GHC.TypeNats

-- Error type for typechecking errors
-- is a monad

type ErrorT = ExceptT String

throwErrorLift :: MonadError e m' => e -> m' a 
throwErrorLift = liftEither . throwError

-- Custome state for type context and value context
-- As long as the getter and setter functions are used 
-- this is can be considered as abstract
type TEnv = [(Name, Type)]
type VEnv  = [(Name, Val)]


insertType :: (Name, Type) -> GEnv -> GEnv
insertType xt genv = genv { typeEnv =  xt : typeEnv genv}

insertVal :: (Name, Val) -> GEnv -> GEnv
insertVal nv genv = genv {nameEnv = nv : nameEnv genv} 

getType :: GEnv -> TEnv
getType = typeEnv

getVal :: GEnv -> VEnv
getVal = nameEnv

getZmodN :: GEnv -> Maybe Number
getZmodN = zmodn

getZmodF :: GEnv -> Maybe CPolyMulti
getZmodF = zmodf

data GEnv = GEnv { typeEnv :: TEnv
                 , nameEnv :: VEnv
                 , zmodn :: Maybe Number
                 , zmodf :: Maybe CPolyMulti }

emptyEnv :: GEnv
emptyEnv = GEnv [] [] Nothing Nothing

type GState = State GEnv

type GStateT = StateT GEnv
