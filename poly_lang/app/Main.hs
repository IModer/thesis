{-# LANGUAGE LambdaCase  #-}
import System.Environment
import System.IO
import Control.Monad (unless)
import Text.Megaparsec.Error
import Control.Monad.State.Lazy  --StateT

-- Saját imports

import Core
import Ring
import Parser
import qualified CoreDepr as CD -- Remove

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

{-
runRepl is ::   (read : IO String) -> 
                (eval : String -> String) -> 
                (print : String -> IO ()) ->
                (runRepl/loop : IO ()) 
-}


--  :q - quit is handled here
{-
runRepl :: IO ()
runRepl = do
    input <- read_
    unless (input == ":q") $ do
        let tm = eval_ input in do 
            print_ tm
            runRepl 
-}


read_ :: IO String
read_ = do
    putStr "poly> "
    hFlush stdout
    getLine

-- Handle repl option like : :q - quit, :h - help, ...
-- Depr
{-
eval_ :: String -> String
eval_ (':':'h':_) = "This is a help"
eval_ cs          = case parseString cs of
    Left a  -> errorBundlePretty a
    Right t -> maybe
                "Typechecking failed"
                CD.prettyPrint
                $ CD.runTypedTerm t
-}

eval :: String -> State SEnv String 
eval cs = case parseString cs of
    Left a   -> return "Parse error"        -- TODO : print errors
    Right tm -> helper (runTypedTerm tm)

        where
            -- TODO redo this, without LambdaCase
            helper :: StateT SEnv Maybe Tm -> State SEnv String
            helper st = do
                env <- get
                mapStateT (\case 
                    (Just (tm',env')) -> return (show tm',env')
                    Nothing -> return ("Type error",env) ) st


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