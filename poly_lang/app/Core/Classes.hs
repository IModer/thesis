module Core.Classes where

import Control.Monad.State
import Control.Monad.Except
import Core.AST
import Core.Types
import GHC.TypeNats
import Data.Text hiding (map, all)

import Data.Maybe (isJust)

-- Error type for typechecking errors

type ErrorT = ExceptT String

--throwErrorLift :: MonadError e m' => e -> m' a 
--throwErrorLift = liftEither . throwError

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

getZmodN :: GEnv -> Maybe (Complex Frac)
getZmodN = zmodn

getZmodF :: GEnv -> Maybe (PolyMulti (Complex Frac))
getZmodF = zmodf

setZmodN :: Maybe (Complex Frac) -> GEnv -> GEnv
setZmodN mf g = g {zmodn = mf}

setZmodF :: Maybe (PolyMulti (Complex Frac)) -> GEnv -> GEnv
setZmodF mp g = g {zmodf = mp}

isContextOpen :: GState Bool
isContextOpen = do
    env <- get
    return $ isJust (getZmodN env) || isJust (getZmodF env)

{-
data GEnv = GEnv { typeEnv :: TEnv
                 , nameEnv :: VEnv
                 , zmodn :: Maybe Frac
                 , zmodf :: Maybe (PolyMulti Frac) }
-}

imag'' :: Val -> ErrorT GState Val
imag'' (VCNum p) = return $ VCNum (imag p :+ 0 %% 1)
imag'' a         = return $ VVar (pack "builtin.imag") ... VVar (pack "x")

imagType = (pack "imag", TCNum ~> TCNum)
imagVal  = (pack "imag", VLam (pack "x") TCNum $ \x ->  
                            imag'' x)

real'' :: Val -> ErrorT GState Val
real'' (VCNum p) = return $ VCNum (real p :+ 0 %% 1)
real'' a         = return $ VVar (pack "builtin.real") ... VVar (pack "x")

realType = (pack "real", TCNum ~> TCNum)
realVal  = (pack "real", VLam (pack "x") TCNum $ \x ->  
                            real'' x)

irred'' :: Val -> ErrorT GState Val
irred'' (VCPoly p) = if polyIsZx p
                        then case monoPolyToZx p of
                            Just (p' , i) -> return $ VBool $ irred' p'
                            Nothing       -> throwError "Runtime error : Argument to irred should be a polinomian with a single varable and coeffs in Z"
                        else throwError "Runtime error : Argument to irred should be a polinomian with a single varable and coeffs in Z"
irred'' a          = return $ VVar (pack "builtin.irred") ... VVar (pack "p")

irredType = (pack "irred", TCPoly ~> TBool)
irredVal  = (pack "irred", VLam (pack "p") TCPoly $ \p ->  
                            irred'' p)

factor'' :: Val -> ErrorT GState Val
factor'' (VCPoly p) = if polyIsZx p
                        then case monoPolyToZx p of
                            Just (p' , i) -> return $ VList $ listToList $ map (VCPoly . flip zxToMultiPoly i) (factor' p')
                            Nothing       -> throwError "Runtime error : Argument to factor should be a polinomian with a single varable and coeffs in Z"
                        else throwError "Runtime error : Argument to factor should be a polinomian with a single varable and coeffs in Z"
factor'' a          = return $ VVar (pack "builtin.factor") ... VVar (pack "p")

factorType = (pack "factor", TCPoly ~> TList)
factorVal  = (pack "factor", VLam (pack "p") TCPoly $ \p ->  
                            factor'' p)

-- Maybe this should fail, or we just check if this is empty list
listToEvalInp :: [Val] -> [(PolyMulti (Complex Frac), Complex Frac)]
listToEvalInp []       = []
listToEvalInp [x]      = []
listToEvalInp (x:y:xs) = case (x,y) of
    (VCPoly p, VCNum n) -> (p , n) : listToEvalInp xs
    (a       , b)       -> listToEvalInp xs

eval'' :: Val -> Val -> ErrorT GState Val
eval'' (VList l) (VCPoly p) = let l' = listToEvalInp $ listToList' l in
                                if l' /= [] && all ((`loneVarOf` p) . fst) l' 
                                    then return $ VCNum $ eval' l' p
                                    else throwError "Runtime error : List given to eval must have the appopriate format : TODO"
eval'' a          b         = return $ (VVar (pack "builtin.eval") ... VVar (pack "ls")) ... VVar (pack "p")

evalType = (pack "eval", TList ~> TCPoly ~> TCNum)
evalVal  = (pack "eval", VLam (pack "ls") TList $ \ls -> 
                        return $ VLam (pack "p") TCPoly $ \p -> 
                            eval'' ls p)

deriv'' :: Val -> Val -> ErrorT GState Val
deriv'' (VCPoly i) (VCPoly p) = if loneVarOf i p
                                    then return $ 
                                         VCPoly $ 
                                         derivativeVar i p
                                    else throwError "Runtime error : Variable to take derivative in must be a single variable thats present in the polinome"
deriv'' a         b          = return $ 
                                (VVar (pack "builtin.derivative") ... VVar (pack "x")) ... VVar (pack "p")

derivType = (pack "derivative", TCPoly ~> TCPoly ~> TCPoly)
derivVal  = (pack "derivative", VLam (pack "x") TCPoly $ \x ->
                        return $ VLam (pack "p") TCPoly $ \p ->
                            deriv'' x p)

emptyEnv :: GEnv
emptyEnv = GEnv [derivType, evalType, factorType, irredType, realType, imagType]
                [derivVal, evalVal, factorVal, irredVal, realVal, imagVal] Nothing Nothing

type GState = State GEnv

type GStateT = StateT GEnv