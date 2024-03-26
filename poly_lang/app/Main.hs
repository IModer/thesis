{-# LANGUAGE LambdaCase, OverloadedStrings #-}
import System.Environment
import System.IO
import System.Directory
import Control.Monad (unless)
import Control.Monad.State.Lazy  --StateT
import Control.Monad.Except      -- ExceptT
import Data.Functor.Identity

import Text.Megaparsec.Error
-- Saját imports

import Core.TypeChecker
import Core.Interpreter
import Core.Classes
import Core.AST
import Ring
import Lib
import Parser
import Data.Text hiding (length, map, unlines)

main :: IO ()
main = do
    args <- getArgs
    env <- handleArgs args
    runRepl env

handleArgs ::  [String] -> IO (GEnv)
handleArgs xs = case pCommandLineCommand xs of
    PrintHelpCL      -> do
        putStrLn help
        return emptyEnv
    NoSuchCommandCl  -> do
        putStrLn "No such command"
        putStrLn help
        return emptyEnv
    GetInfoCL topic  -> do
        putStrLn $ helpOnTopic topic
        return emptyEnv
    -- Ezt lehet ki lehetne absztrahálni
    LoadFileCL files -> do
        (logs, env) <- runStateT (loadFiles files) (GEnv [] [])
        forM logs putStrLn
        return env

-- List of filenames, we open each one and we parse the contents
loadFiles :: [String] -> GStateT IO [String]
loadFiles [] = return ["No files loaded"]
loadFiles filenames = do
    forM filenames processFile
    where
        processFile :: String -> GStateT IO String
        processFile filename = do
            b <- lift $ doesFileExist filename
            if b
                then do
                    con <- lift $ readFile filename
                    s <- evalFile filename con
                    return ("Loading file : " ++ filename ++ "\n" ++ s)
                else do
                    return $ "There is no such file : " ++ filename

help :: String
help = "Usage : poly_lang.exe <command>   \n\
                      \Commands: \tload <file1> [<file2> ...] - loads the specified files\n\
                      \ \t\thelp - prints this help\n\
                      \ \t\tdocs <topic> - Prints help on the specified topic, use `docs topis` to print out the available topics"

helpOnTopic :: Topic -> String
helpOnTopic = \case
    MetaTopic -> "Current topics: \n\t\tDummy\n\t\ttopic"
    Dummy     -> "This is the docs of Dummy. Its me i am the dummy :D"
    _         -> "No such topic, run `docs topic` to see avaliable topics\n"

read_ :: IO String
read_ = do
    putStr "poly> "
    hFlush stdout
    getLine

evalFile :: String -> String -> GStateT IO String
evalFile filename cs = case parseStringFile filename $ pack cs of
    Left a -> return $ errorBundlePretty a
    Right tms_defs -> do
        s <- mapM handleTmDef tms_defs
        return $ unlines s
    where
        handleTmDef :: Either TTm TopDef -> GStateT IO String
        handleTmDef tm_def = case tm_def of
            Left tm   -> do
                lift $ putStrLn $ "running Tm : " ++ show tm
                handleErrorTm' (runTypedTerm tm)
            Right def -> do
                handleErrorString' (handleTopDef def)

evalRepl :: String -> GStateT IO String
evalRepl cs = case parseStringRepl $ pack cs of
    Left a   -> return $ errorBundlePretty a
    Right tm_co_def -> case tm_co_def of
        OLeft tm -> do
            handleErrorTm' (runTypedTerm tm)
        OMiddle co -> handleCommand co
        ORight def -> do
            handleErrorString' (handleTopDef def)

handleErrorTm' :: ErrorT GState Tm -> GStateT IO String
handleErrorTm' e = 
    let a = runExceptT e in 
    mapStateT (\i -> 
        let (e_tm, env) = runIdentity i in 
        return (eitherIdL show e_tm,env)) a

handleErrorString' :: ErrorT GState String -> GStateT IO String
handleErrorString' e = 
    let a = runExceptT e in
    mapStateT (\i -> 
        let (e_s, env) = runIdentity i in 
        return (eitherId e_s, env)) a

handleErrorString :: GEnv -> Error (String, GEnv) -> IO (String, GEnv)
handleErrorString env = either 
                            (\e -> return (e, env)) 
                            (return)

handleErrorTm :: GEnv -> Error (Tm , GEnv) -> IO (String, GEnv)
handleErrorTm env = either
                            (\e  -> return (e, env))
                            (\(tm,env') -> return (show tm, env'))
{-
    case m of
    Just (tm',env') -> return (show tm',env')
    Nothing         -> return ("Type error",env)
-}

handleTopDef :: TopDef -> ErrorT GState String
handleTopDef def = case def of
    LetDef name ttm -> do
        t <- typeCheck [] ttm
        val <- lift $ evalTerm [] (loseType ttm)
        modify $ insertType (name, t)
        modify $ insertVal (name, val)
        return ("saved " ++ unpack name ++ " : " ++show t)
    VarDef name    -> do
        modify $ insertType (name, TPoly)
        modify $ insertVal (name, VPolyVar name)
        return $ unpack name ++ " is now a polinomial variable"

-- This type is not strong enough, cos here we have to do a lot of things like IO, ...
handleCommand :: Command -> GStateT IO String
handleCommand co = case co of
    PrintHelp   -> return help
    RunTimed tm -> undefined -- run tm and print out measure the time it took
    LoadFile ns -> unlines <$> loadFiles (map unpack ns) -- load files then 
    GetType  tm -> undefined -- we have to typecheck tm then print out the type 
    GetInfo  tp -> return $ helpOnTopic tp

print_ :: String -> IO ()
print_ = putStrLn

runRepl :: GEnv -> IO ()
runRepl = evalStateT runStatefulRepl

runStatefulRepl :: GStateT IO ()
runStatefulRepl = do
    inp <- lift read_                           -- Lift IO into StateT IO
    unless (trimS inp == ":q") $ do
        tm <- evalRepl inp                      -- Eval could print everything so we dont need to worry about passing things to print_
        lift $ print_ tm                        -- Lift IO into StateT IO
        runStatefulRepl
    where
        trimS = unpack . strip . pack