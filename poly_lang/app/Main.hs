{-# LANGUAGE LambdaCase, OverloadedStrings #-}
import System.Environment
import System.IO
import System.Directory
import System.Clock
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
import Core.Types
import Lib
import Parser
import Data.Text hiding (length, map, unlines)

main :: IO ()
main = do
    args <- getArgs
    env <- handleArgs args
    runRepl env

handleArgs ::  [String] -> IO GEnv
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
        (logs, env) <- runStateT (loadFiles files) (emptyEnv)
        forM_ logs putStrLn
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
                      \ \t\tdocs <topic> - Prints help on the specified topic, use `docs topic` to print out the available topics"

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
-- TODO : le kell kezelni ha a kifejezés nem volt helyes,
--  hogy akkor az egész ne töltsön be
            Left tm   -> do
--                lift $ putStrLn $ "running Tm : " ++ show tm
                handleErrorShow (runTypedTerm tm)
            Right def -> do
                handleErrorString (handleTopDef def)

evalRepl :: String -> GStateT IO String
evalRepl cs = case parseStringRepl $ pack cs of
    Left a   -> return $ errorBundlePretty a
    Right tm_co_def_b -> 
        case tm_co_def_b of
            Left tm_def_com -> 
                case tm_def_com of
                    OLeft tm    -> handleErrorShow (runTypedTerm tm)
                    OMiddle co  -> handleCommand co
                    ORight def  -> handleErrorString (handleTopDef def)
            Right b         -> return "" -- it was an empty line

handleErrorShow :: Show a => ErrorT GState a -> GStateT IO String
handleErrorShow e = 
    let a = runExceptT e in 
    mapStateT (\i -> 
        let (e_tm, env) = runIdentity i in 
        return (eitherIdL show e_tm,env)) a

handleErrorString :: ErrorT GState String -> GStateT IO String
handleErrorString e =
    let a = runExceptT e in
    mapStateT (\i -> 
        let (e_s, env) = runIdentity i in
        return (eitherId e_s, env)) a

handleTopDef :: TopDef -> ErrorT GState String
handleTopDef def = case def of
    LetDef name ttm -> do
        t <- typeCheck [] ttm
        --val <- lift $ evalTerm [] ttm
        val <- evalTerm [] ttm
        modify $ insertType (name, t)
        modify $ insertVal (name, val)
        return ("saved " ++ unpack name ++ " : " ++ show t)
    VarDef name    -> do
        modify $ insertType (name, TCPoly)
        case stringToComplexPoly $ unpack name of
            Just p -> do
                modify $ insertVal (name, VCPoly p)
                return $ unpack name ++ " is now a polinomial variable"
            Nothing -> throwErrorLift (unpack name ++ " cannot be made a polinomial variable")
    OpenDef a -> undefined
    CloseDef -> undefined

handleCommand :: Command -> GStateT IO String
handleCommand co = case co of
    PrintHelp   -> return help
    RunTimed tm -> do  -- run tm and print out measure the time it took
        t1 <- lift $ getTime Monotonic
        rs <- handleErrorShow (runTypedTerm tm)
        t2 <- lift $ getTime Monotonic
        return $ rs ++ "\nrunning it took: " ++ show (toNanoSecs $ diffTimeSpec t1 t2) ++ " ns"
    LoadFile ns -> unlines <$> loadFiles (map unpack ns) -- load files then 
    GetType  tm -> handleErrorShow (typeCheck [] tm)  -- we have to typecheck tm then print out the type
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
{-
        if trimS inp /= "" then do
            tm <- evalRepl inp                      -- Eval could print everything so we dont need to worry about passing things to print_
            lift $ print_ tm                        -- Lift IO into StateT IO
            runStatefulRepl
        else do
            runStatefulRepl
-}
    where
        trimS = unpack . strip . pack