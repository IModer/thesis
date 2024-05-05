module Core.Classes where

import Control.Monad.State (State, StateT, get)
import Control.Monad.Except (ExceptT, throwError)
import Core.AST   -- innen is
import Core.Types -- TODO valszeg minden kell
import Data.Text hiding (map, all)

import Data.Maybe (isJust)

-- Error type for typechecking errors

type ErrorT = ExceptT String


type GState = State GEnv

type GStateT = StateT GEnv

-- Custom state for type context and value context
-- As long as the getter and setter functions are used 
-- this can be considered as abstract
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

getFiles :: GEnv -> [String]
getFiles = files

setFiles :: [String] -> GEnv -> GEnv
setFiles fs g = g {files = fs}

modifyFiles :: ([String] -> [String]) -> GEnv -> GEnv
modifyFiles f g = let fs' = f $ files g in g {files = fs'}

isContextOpen :: GState Bool
isContextOpen = do
    env <- get
    return $ isJust (getZmodN env) || isJust (getZmodF env)

{- Defined in Core.AST
data GEnv = GEnv { typeEnv :: TEnv
                 , nameEnv :: VEnv
                 , zmodn :: Maybe Frac
                 , zmodf :: Maybe (PolyMulti Frac)
                 , names :: [Strings] }
-}

imag'' :: Val -> ErrorT GState Val
imag'' (VCNum p) = return $ VCNum (imag p :+ 0 %% 1)
imag'' _         = return $ VVar (pack "builtin.imag") ... VVar (pack "x")

imagType :: (Name, Type)
imagVal  :: (Name, Val)
imagType = (pack "imag", TCNum ~> TCNum)
imagVal  = (pack "imag", VLam (pack "x") TCNum $ \x ->  
                            imag'' x)

real'' :: Val -> ErrorT GState Val
real'' (VCNum p) = return $ VCNum (real p :+ 0 %% 1)
real'' _         = return $ VVar (pack "builtin.real") ... VVar (pack "x")

realType :: (Name, Type)
realVal  :: (Name, Val)
realType = (pack "real", TCNum ~> TCNum)
realVal  = (pack "real", VLam (pack "x") TCNum $ \x ->  
                            real'' x)

listToSubstInp :: [Val] -> Maybe ([(PolyMulti (Complex Frac), PolyMulti (Complex Frac))])
listToSubstInp []       = Just []
listToSubstInp [_]      = Nothing
listToSubstInp (x:y:xs) = case (x,y) of
    (VCPoly p, VCPoly n) -> do
        xs' <- listToSubstInp xs
        return $ (p , n) : xs'
    _                    -> listToSubstInp xs

subst'' :: Val -> Val -> ErrorT GState Val
subst'' (VList l) (VCPoly p) = let l' = listToSubstInp l in
    case l' of
        Just ls | all ((`loneVarOf` p) . fst) ls -> return $ VCPoly $ subst' ls p
        _                                        -> throwError "Runtime error : List given to subst must have the appropriate format, for more details type\n :i Polinomials"
subst'' _          _         = return $ (VVar (pack "builtin.subst") ... VVar (pack "ls")) ... VVar (pack "p")

substType :: (Name, Type)
substVal  :: (Name, Val)
substType = (pack "subst", TList ~> TCPoly ~> TCPoly)
substVal  = (pack "subst", VLam (pack "ls") TList $ \ls -> 
                        return $ VLam (pack "p") TCPoly $ \p -> 
                            subst'' ls p)

irred'' :: Val -> ErrorT GState Val
irred'' (VCPoly p) = if polyIsZx p
                        then case monoPolyToZx p of
                            Just (p' , _) -> return $ VBool $ irred' p'
                            Nothing       -> throwError "Runtime error : Argument to irred should be a polinomian with a single varable and coeffs in Z"
                        else throwError "Runtime error : Argument to irred should be a polinomian with a single varable and coeffs in Z"
irred'' _          = return $ VVar (pack "builtin.irred") ... VVar (pack "p")

irredType :: (Name, Type)
irredVal  :: (Name, Val)
irredType = (pack "irred", TCPoly ~> TBool)
irredVal  = (pack "irred", VLam (pack "p") TCPoly $ \p ->  
                            irred'' p)

factor'' :: Val -> ErrorT GState Val
factor'' (VCPoly p) = if polyIsZx p
                        then case monoPolyToZx p of
                            Just (p' , i) -> return $ VList $ map (VCPoly . flip zxToMultiPoly i) (factor' p')
                            Nothing       -> throwError "Runtime error : Argument to factor should be a polinomian with a single varable and coeffs in Z"
                        else throwError "Runtime error : Argument to factor should be a polinomian with a single varable and coeffs in Z"
factor'' _          = return $ VVar (pack "builtin.factor") ... VVar (pack "p")

factorType :: (Name, Type)
factorVal  :: (Name, Val)
factorType = (pack "factor", TCPoly ~> TList)
factorVal  = (pack "factor", VLam (pack "p") TCPoly $ \p ->  
                            factor'' p)

listToEvalInp :: [Val] -> Maybe ([(PolyMulti (Complex Frac), Complex Frac)])
listToEvalInp []       = Just []
listToEvalInp [_]      = Nothing
listToEvalInp (x:y:xs) = case (x,y) of
    (VCPoly p, VCNum n) -> do
        xs' <- listToEvalInp xs
        return $ (p , n) : xs'
    _                   -> listToEvalInp xs

eval'' :: Val -> Val -> ErrorT GState Val
eval'' (VList l) (VCPoly p) = let l' = listToEvalInp l in
    case l' of
        Just ls | all ((`loneVarOf` p) . fst) ls -> return $ VCNum $ eval' ls p
        _ -> throwError "Runtime error : List given to eval must have the appropriate format, for more details type\n :i Polinomials"
eval'' _          _         = return $ (VVar (pack "builtin.eval") ... VVar (pack "ls")) ... VVar (pack "p")

evalType :: (Name, Type)
evalVal  :: (Name, Val)
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
deriv'' _         _           = return $ (VVar (pack "builtin.derivative") ... VVar (pack "x")) ... VVar (pack "p")

derivType :: (Name, Type)
derivVal  :: (Name, Val)
derivType = (pack "derivative", TCPoly ~> TCPoly ~> TCPoly)
derivVal  = (pack "derivative", VLam (pack "x") TCPoly $ \x ->
                        return $ VLam (pack "p") TCPoly $ \p ->
                            deriv'' x p)

emptyEnv :: GEnv
emptyEnv = GEnv [derivType  --types of builtins
                , evalType
                , factorType
                , irredType
                , realType
                , imagType
                , substType
                ]           -- vals of buildtins
                [derivVal
                , evalVal
                , factorVal
                , irredVal
                , realVal
                , imagVal
                , substVal
                ]
                Nothing Nothing -- we start with an empty context
                ["..\\Prelude.poly"] -- always load prelude