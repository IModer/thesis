import System.Environment
import System.IO
import Control.Monad (unless)
import Text.Megaparsec.Error

-- Saját imports

import Core
import Ring
import Parser

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
    unless (input == ":q") $ do
        let tm = eval_ input in do 
            print_ tm
            runRepl 


read_ :: IO String
read_ = do
    putStr "poly> "
    hFlush stdout
    getLine

--eval_ :: String -> String
-- Handle repl option like : :q - quit, :h - help, ...
eval_ :: String -> String
eval_ (':':'h':_) = "This is a help"
eval_ cs          = case parseString cs of
    Left a  -> errorBundlePretty a
    Right t -> case runTypedTerm t of 
        Just t' -> prettyPrint t' 
        Nothing -> "Typechecking failed"
{- TODO
eval_ cs          = do 
    term <- parseTerm cs
    normalizeTerm term
-}

print_ :: String -> IO ()
print_ = putStrLn


-----------------------------------------------------------------------
---------------------------Playing with State--------------------------
-----------------------------------------------------------------------

type SEnv = (TEnv , Env) 

runRepl' :: IO ()
runRepl' = runStatefulRepl ([],[])

runStatefulRepl :: SEnv -> IO ()
runStatefulRepl env = do
    inp <- read_
    unless (inp == ":q") $ do
        let (env', tm) = eval env inp in  -- This could fail, for now let it be String
            do 
                print_ tm
                runStatefulRepl env'

eval :: SEnv -> String -> (SEnv, String)
eval env cs = case parseString cs of
    Left a    -> (env, "Parse error")
    Right tm  -> case runTypedTerm' env tm of
        (env', Just tm') -> (env', prettyPrint tm')
        (env', Nothing)  -> (env', "Typechecking error")

runTypedTerm' :: SEnv -> TTm -> (SEnv, Maybe Tm)
runTypedTerm' env tm = do
    let (env', t) = typeCheck' env tm in
        case t of 
            Nothing -> (env, Nothing)  -- régi env, mert failelt a typeCheck
            Just t -> let (env'', tm') = normalForm' env' $ loseType tm in (env'', Just tm')

typeCheck' :: SEnv -> TTm -> (SEnv, Maybe Type)
typeCheck' = undefined

-- :: Tm -> State SEnv Tm
normalForm' :: SEnv -> Tm -> (SEnv, Tm)
normalForm' = undefined

-- :: Val -> State SEnv Tm
quoteTerm' :: SEnv -> Val -> (SEnv, Tm) 
quoteTerm' = undefined
{-

runTypedTerm :: Env -> TTm -> (Env, Maybe Tm)
runTypedTerm env tm = do
    _ <- typeCheck env tm
    return $ normalForm env $ loseType tm


typeCheck most :: TEnv -> TTm -> Maybe Type

typeCheck :: Env -> TTm -> (Env, Maybe Type)
typeChekk (tenv, env) = ... (csak tenv et használja és módosítja)

normalForm :: Env -> Tm -> (Env, Tm)

quoteTerm :: Env -> Val -> (Env, Tm)   -- ő pl nem módosítja az Env-et
qouteTerm env = ... (mostani kód)
    where
        ns = map (fst . fst) env

evalTerm :: Env -> Tm -> (Env, Val)

print_ .: String -> IO ()
print_ = putStrLn



-}