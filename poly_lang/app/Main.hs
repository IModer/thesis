{-# LANGUAGE LambdaCase  #-}
import System.Environment
import System.IO
import Control.Monad (unless)
import Control.Monad.State.Lazy  --StateT
import Data.Functor.Identity

import Text.Megaparsec.Error
-- Saját imports

import Core
import Ring
import Lib
import Parser
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    parseArgs args
    runRepl

parseArgs :: [String] -> IO ()
parseArgs ("load":files)   = loadFiles files
parseArgs ("help":_)       = printHelp
parseArgs ("docs":topic:_) = printHelpOnTopic topic
parseArgs _                = putStrLn "No such command\n" >> printHelp

loadFiles :: [String] -> IO ()
loadFiles []     = putStrLn ""
loadFiles (x:xs) = do
    con <- readFile x
    putStrLn con
    loadFiles xs

{- something something fmap
loadFile xs = do 
    content <- map readFile xs
    print "asd"
-}

printHelp :: IO ()
printHelp = putStrLn "Usage : poly_lang.exe <command>   \n\
                      \Commands: \tload <file1> [<file2> ...] - loads the specified files\n\
                      \ \t\thelp - prints this help\n\
                      \ \t\tdocs <topic> - Prints help on the specified topic, use `docs topis` to print out the available topics"

printHelpOnTopic :: String -> IO ()
printHelpOnTopic "topic" = putStrLn "Current topics: \n\t\tDummy\n\t\ttopic"
printHelpOnTopic "Dummy" = putStrLn "This is the docs of Dummy. Its me i am the dummy :D"
printHelpOnTopic _       = putStrLn "No such topic, run `docs topic` to see avaliable topics\n"

read_ :: IO String
read_ = do
    putStr "poly> "
    hFlush stdout
    getLine

-- TODO : :: String -> State GEnv String
-- GEnv :: [(Name, (Type, Tm))]
-- GEnv be a VarDef és LetDef topdef ek pakolnak 
--      és a runTypedTerm használja 
-- kell : executeCommand, executeTopDef, 
--        utóbbi akár inline is lehet
eval :: String -> State SEnv String
eval cs = case parseString $ T.pack cs of
    Left a   -> return $ errorBundlePretty a        -- TODO : print errors
    Right tm_co -> case tm_co of
        OLeft tm -> do
            env <- get
            mapStateT (handleMaybeTm env) (runTypedTerm tm)
        OMiddle co -> handleCommand co
        ORight def -> do
            env <- get
            --mapStateT (maybe (Identity ("Type error", env)) (Identity . id)) (handleTopDef def)
            mapStateT (handleMaybeString env) (handleTopDef def)

-- maybe :: b -> (a -> b) -> Maybe a -> b
-- a = (String,SEnv)
-- b = (String,SEnv)
-- maybe :: (String,SEnv) -> ((String,SEnv) -> (String,SEnv)) -> Maybe (String,SEnv) -> (String,SEnv)
-- maybe ("Type error", env) (id) (handleTopDef def) 
handleMaybeString :: SEnv -> Maybe (String, SEnv) -> Identity (String, SEnv)
handleMaybeString env = maybe (Identity ("Type error", env)) (Identity . id)

handleMaybeTm :: SEnv -> Maybe (Tm , SEnv) -> Identity (String, SEnv)
handleMaybeTm env m = case m of
    Just (tm',env') -> return (show tm',env')
    Nothing         -> return ("Type error",env)

handleTopDef :: TopDef -> StateT SEnv Maybe String
handleTopDef def = case def of
    -- We typecheck and eval ttm and then store it in Env
    LetDef name ttm -> do
        t <- (typeCheck [] ttm)
        val <- (state . runState) $ evalTerm (loseType ttm)
        modify $ insertType (name, t)
        modify $ insertName (name, val)
        return ("saved " ++ T.unpack name)
    -- We save the fact that name has type Poly 
    -- and save it as polyvar 
    VarDef name    -> do
        modify $ insertType (name, TTop) -- TODO : No TTop
        modify $ insertName (name, VLit $ LTop)
        return ""
    where


handleCommand :: Command -> State SEnv String
handleCommand = undefined

print_ :: String -> IO ()
print_ = putStrLn

runRepl :: IO ()
runRepl = evalStateT runStatefulRepl $ SEnv [] []

runStatefulRepl :: StateT SEnv IO ()
runStatefulRepl = do
    inp <- lift read_                           -- Lift IO into StateT IO
    unless (inp == ":q") $ do
        tm <- (state . runState) $ eval inp     -- Unbox and box (StateT Id -> StateT IO)
        lift $ print_ tm                        -- Lift IO into StateT IO
        runStatefulRepl
