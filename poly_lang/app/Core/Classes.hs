module Core.Classes where

import Control.Monad.State
import Control.Monad.Except
import Core.AST
import Core.Types
import GHC.TypeNats
import Data.Text

-- Error type for typechecking errors
-- is a monad

type ErrorT = ExceptT String

throwErrorLift :: MonadError e m' => e -> m' a 
throwErrorLift = liftEither . throwError

-- Custome state for type context and value context
-- As long as the getter and setter functions are used 
-- this is can be considered as abstract
--type TEnv = [(Name, Type)]
--type VEnv  = [(Name, Val)]

insertType :: (Name, Type) -> GEnv -> GEnv
insertType xt genv = genv { typeEnv =  xt : typeEnv genv}

insertVal :: (Name, Val) -> GEnv -> GEnv
insertVal nv genv = genv {nameEnv = nv : nameEnv genv} 

getType :: GEnv -> TEnv
getType = typeEnv

getVal :: GEnv -> VEnv
getVal = nameEnv

getZmodN :: GEnv -> Maybe Frac
getZmodN = zmodn

getZmodF :: GEnv -> Maybe (PolyMulti Frac)
getZmodF = zmodf

{-
data GEnv = GEnv { typeEnv :: TEnv
                 , nameEnv :: VEnv
                 , zmodn :: Maybe Frac
                 , zmodf :: Maybe (PolyMulti Frac) }
-}

deriv'' :: Val -> Val -> ErrorT GState Val
deriv'' (VCPoly i) (VCPoly p) = if loneVarOf i p
                                    then return $ VCPoly $ derivativeVar i p
                                    else throwError "Runtime error : Variable to take derivative in must be a single variable thats present in the polinome"
deriv'' a         b          = return $ VApp (VApp (VVar $ pack "builtin.derivative") (VVar $ pack "x")) (VVar $ pack "p")

derivType = (pack "derivative",TArr TCPoly $ TArr TCPoly TCPoly)
derivVal  = (pack "derivative", VLam (pack "x") TCPoly $ \x -> 
                        return $ VLam (pack "p") TCPoly $ \p -> 
                            deriv'' x p)

emptyEnv :: GEnv
emptyEnv = GEnv [derivType] 
                [derivVal] Nothing Nothing

type GState = State GEnv

type GStateT = StateT GEnv


instance Monoid (ExceptT String GState String) where
    mempty = return ""
    mappend a b = do
        a' <- a
        b' <- b
        return $ a' ++ b'

instance Semigroup (ExceptT String GState String) where
    (<>) = mappend