import System.Environment
import System.IO
import Control.Monad (unless)

-- Saját imports

import Core
import Ring

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
runRepl :: IO ()
runRepl = do
    input <- read_
--TODO : ask about this magic
-- why doesnt this work with do
    unless (input == ":q")
       $ print_ (eval_ input)
      >> runRepl

read_ :: IO String
read_ = do
    putStr "poly> "
    hFlush stdout
    getLine

--eval_ :: String -> String
-- Handle repl option like : :q - quit, :h - help, ...
eval_ :: [Char] -> String
eval_ (':':'h':_) = "This is a help"
eval_ cs          = cs
{- TODO
eval_ cs          = do 
    term <- parseTerm cs
    normalizeTerm term
-}

print_ :: String -> IO ()
print_ = putStrLn