{-# LANGUAGE LambdaCase  #-}
import System.Environment
import System.IO
import System.Directory
import Control.Monad (unless)
import Control.Monad.State.Lazy  --StateT
import Data.Functor.Identity

import Text.Megaparsec.Error
-- Saját imports

import Core
import Ring
import Lib
import Parser
import qualified Data.Text as T hiding (length)

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
    runRepl

parseArgs :: [String] -> IO ()
--parseArgs ("load":files)   = loadFiles files
parseArgs ("help":_)       = putStrLn help
parseArgs ("docs":topic:_) = putStrLn $ helpOnTopic undefined -- here : topic -(parsing with pInfoTopic)-> ?
parseArgs _                = putStrLn "No such command\n" >> putStrLn help

-- List of filenames, we open each one and we parse the contents
loadFiles :: [String] -> StateT SEnv IO [String]
loadFiles [] = return ["No files loaded"]
loadFiles filenames = do
    forM filenames processFile
    where
        processFile :: String -> StateT SEnv IO String
        processFile filename = do
            b <- lift $ doesFileExist filename
            if b
                then do
                    con <- lift $ readFile filename
                    s <- mapM eval (lines con)
                    return $ ("Loading file : " ++ filename ++ "\n" ++ head s)
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

-- SEnv should have a Error part where we can signal is the parsing wasnt okay
eval :: String -> StateT SEnv IO String
eval cs = case parseString "poly" $ T.pack cs of
    Left a   -> return $ errorBundlePretty a        -- TODO : print errors
    Right tm_co -> case tm_co of
        OLeft tm -> do
            env <- get
            mapStateT (handleMaybeTm env) (runTypedTerm tm)
        OMiddle co -> handleCommand co
        ORight def -> do
            env <- get
            mapStateT (handleMaybeString env) (handleTopDef def)

handleMaybeString :: SEnv -> Maybe (String, SEnv) -> IO (String, SEnv)
handleMaybeString env = maybe (return ("Type error", env)) (return . id)

handleMaybeTm :: SEnv -> Maybe (Tm , SEnv) -> IO (String, SEnv)
handleMaybeTm env m = case m of
    Just (tm',env') -> return (show tm',env')
    Nothing         -> return ("Type error",env)

handleTopDef :: TopDef -> StateT SEnv Maybe String
handleTopDef def = case def of
    LetDef name ttm -> do
        t <- (typeCheck [] ttm)
        val <- (state . runState) $ evalTerm (loseType ttm)
        modify $ insertType (name, t)
        modify $ insertName (name, val)
        return ("saved " ++ T.unpack name)
    VarDef name    -> do
        modify $ insertType (name, TPoly) -- TODO : No TTop
        modify $ insertName (name, VPolyVar name)
        return ""

-- This type is not strong enough, cos here we have to do a lot of things like IO, ...
handleCommand :: Command -> StateT SEnv IO String
handleCommand co = case co of
    PrintHelp   -> return help
    RunTimed tm -> undefined -- run tm and print out measure the time it took
    LoadFile ns -> unlines <$> loadFiles (map T.unpack ns) -- load files then 
    GetType  tm -> undefined -- we have to typecheck tm then print out the type 
    GetInfo  tp -> return $ helpOnTopic tp

print_ :: String -> IO ()
print_ = putStrLn

runRepl :: IO ()
runRepl = evalStateT runStatefulRepl $ SEnv [] []

runStatefulRepl :: StateT SEnv IO ()
runStatefulRepl = do
    inp <- lift read_                           -- Lift IO into StateT IO
    unless (inp == ":q") $ do
        tm <- eval inp                          -- Eval could print everything so we dont need to worry about passing things to print_
        lift $ print_ tm                        -- Lift IO into StateT IO
        runStatefulRepl
