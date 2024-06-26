{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}
import System.Environment (getArgs)
import System.IO (hFlush, stdout)
import System.Directory (doesFileExist)
import System.Clock (getTime, Clock(..), diffTimeSpec, toNanoSecs)
import Control.Monad.State.Lazy 
    (StateT(..)
    , evalStateT
    , lift
    , unless
    , get
    , modify
    , mapStateT
    , forM
    , forM_)
import Control.Monad.Except (ExceptT(..), throwError, runExceptT)
import Data.Functor.Identity (runIdentity)
import Data.List (union)
import Data.Bifunctor (first)

import Text.Megaparsec.Error
-- Saját imports

import Core.TypeChecker
import Core.Interpreter
import Core.Classes
import Core.AST
import Core.Types
import Lib
import Parser
import qualified Data.Text as T

main :: IO ()
main = do
    args <- getArgs
    handleArgs args

handleArgs ::  [String] -> IO ()
handleArgs [] = do
    putStrLn welcome
    runRepl emptyEnv
handleArgs xs = case pCommandLineCommand xs of
    PrintHelpCL      -> do
        putStrLn help
    NoSuchCommandCl  -> do
        putStrLn "No such command"
        putStrLn help
    GetInfoCL topic  -> do
        putStrLn $ helpOnTopic topic
    LoadFileCL files -> do
        (logs, env) <- runStateT (loadFiles files) emptyEnv
        putStrLn $ welcome ++ "\n"
        forM_ logs putStrLn
        runRepl env
        
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
                    let a = lines s
                    if length a > 10
                        then return ("Loading file : " ++ filename ++ "\n" ++ unlines (take 5 a ++ ["...(" ++ show (length a) ++ ") more lines"]))
                        else return ("Loading file : " ++ filename ++ "\n" ++ s ++ "\n")
                else
                    return $ "There is no such file : " ++ filename

welcome :: String
welcome = "Welcome to poly_lang, a basic computer algebra system\
            \ focused on polinomials. To get started run the command\
            \ `:i ListTopics` then `:i <topic>` to check out whatever\
            \ topic interests you."

help :: String
help = "Usage : poly_lang.exe or poly_lang.exe <command>   \n\
                      \Commands: \tload <file1> [<file2> ...] - loads the specified files\n\
                      \ \t\thelp - prints this help\n\
                      \ \t\tdocs <topic> - Prints help on the specified topic, use `docs topic` to print out the available topics"

helpOnTopic :: Topic -> String
helpOnTopic = \case
    MetaTopic   -> "Topics: " ++ alltopicsPretty 
    Builtins    -> "\n\npoly_lang has several built-in functions that act on polinomials and numbers.\n\n\
                    \ derivative : Poly -> Poly -> Poly - Takes the derivative of a polinomial in a given \
                    \ variable. The variable shuld be the first parameter and the polinomial to derivate the second\
                    \ Examples: \n\n\
                    \ \tpoly> derivative X (X * Y)\n\
                    \ \t1 * Y\n\
                    \ \tpoly> derivative X (X * Y)\n\
                    \ \t1 * X\n\
                    \ \tpoly> derivative X (powP X 10 + X * X + 10)\n\
                    \ \t10 * X^9 + 2 * X\
                    \ eval : List -> Poly -> Num - Evaluates a given polinomial at a given point.\
                    \ The given list should have the format [p_0, n_0, p_1, n_1, ...]. Then each variable p_i\
                    \ will be evaluated to be n_i, if some variable is not specified then it will be evaluated\
                    \ to be 1.\
                    \ Examples: \n\n\
                    \ \tpoly> derivative X (X * Y)\n\
                    \ \t1 * Y\n\
                    \ \tpoly> derivative X (X * Y)\n\
                    \ \t1 * X\n\
                    \ \tpoly> derivative X (powP X 10 + X * X + 10)\n\
                    \ \t10 * X^9 + 2 * X\
                    \ factor : Poly -> List - Factors the given polinomial over Z. \
                    \ Examples: \n\n\
                    \ \tpoly> factor (X * X)\n\
                    \ \t[1 * X,1 * X]\n\
                    \ \tpoly> factor (X * X + 1)\n\
                    \ \t[1 * X^2 + 1]\n\
                    \ \tpoly> factor (X * X - 4)\n\
                    \ \t[1 * X + -2,1 * X + 2]\
                    \ irred : Poly -> Bool - Determines if the given polinomial is irreducable in Z  \
                    \ Examples: \n\n\
                    \ \tpoly> irred (X * X)  \n\
                    \ \tFalse\n\
                    \ \tpoly> factor (X * X)\n\
                    \ \t[1 * X,1 * X]\n\
                    \ subst : List -> Poly -> Poly - Substitutes in polinomials for each variable in a polinomial. \
                    \ The input list has to have the format [p_0, q_0, p_1, q_1, ...].  Then for each p_i variable in\
                    \ the input polinomial, q_i will be substituted.\
                    \ Examples: \n\n\
                    \ \tpoly> subst [X, Y] (X + 1)\n\
                    \ \t1 * Y + 1\n\
                    \ real/imag : Num -> Num - Returns the real or imaginary parts of a complex number \
                    \ Examples: \n\n\
                    \ \tpoly> real (10 + 2 * i)\n\
                    \ \t10\n\
                    \ \tpoly> imag (10 + 2 * i)\n\
                    \ \t2"
    Polinomials -> "\n\nPolinomals in poly_lang represent complex polinomials.\n\
                    \ To use polinomials you have to declare variable with\
                    \ the `var` keyword followed by a capital english letter\
                    \. After that you can construct polinomals using the\
                    \ letter you declared as a variable. Examples: \n\n\
                    \ \tpoly> var X\n\
                    \ \tX is now a polinomial variable\n\
                    \ \tpoly> X * X\n\
                    \ \t1 * X^2"
    Lists       -> "\n\nLists are polymorphic lists that can contain any Term (Program)\
                    \ but cannot be destructed (indexed, or iterated). They can be\
                    \ constructed with the usual list syntax (eg.: [1,2,3]) or with\
                    \ appending elements to the empty list [] (e.: 1 :: 2 :: [])\
                    \ Examples: \n\n\
                    \ \tpoly> 1 :: True :: []\n\
                    \ \t[1,True]\n\
                    \ \tpoly> factor (X * X - 4)\n\
                    \ \t[1 * X + -2,1 * X + 2]\n\
                    \ \tpoly> :t []\n\
                    \ \tList"
    Functions   -> "Fuctions can be created with the lambda symtax: \n\n\
                    \ \t \\Name : Type -> Body\n\n\
                    \where the Name is the name that the bound variable or \"input\" of\
                    \ the funtion will have inside the Body and Type is the type of it.\
                    \ They can be \"called\" or applied with writing their arguments\
                    \ after the function. Functions can be nested and if they have the\
                    \ same name for the bound variable the inner name will shadow the\
                    \ outer one. You can also ignore the Name if you write `_` but you\
                    \ will still have to write its Type and apply a Term of that type\
                    \ to call it.\
                    \ Examples: \n\n\
                    \ \tpoly> \\x : Bool . x\n\
                    \ \t(\\ x : Bool . x )\n\
                    \ \tpoly> (\\x : Bool . x) True\n\
                    \ \tTrue\n\
                    \ \tpoly> (\\_ : Bool . 3) True\n\
                    \ \t3\n\n\
                    \ Alternatively if we have many agrument, or arguments of the same type\
                    \ then can be written in a nicer syntax: \n\
                    \ \t \\x_1 y_1 : Type_1 , x_2 y_2 : Type_2 . Body"
    Commands    -> "The available commands are :\n\
                   \\t:h or :help - Prints help message\n\
                   \\t:q or :quit - Exits the program\n\
                   \\t:i <topic> or :into <topic> - Prints information on a topic\n\
                   \\t:b <tm> or :timeit <tm> - Tries to run the Term tm\
                   \ and measures how much time it took\n\
                   \\t:t <tm> or :type <tm> - Tries to typecheck the Term tm\n\
                   \\t:l <f> [<f2>]* or :load <f> [<f2>]* - Loads the given files"
    TopDefs     -> "The available top level definitions are :\n\
                    \\tvar - Declares a capital letter a poolinomial variable\n\
                    \\tdef - Declares a name to have synonyms with the given value\n\
                    \\topen - Opens a context with a Number or Polinomial,\ 
                    \ while the context is open all operation are performed modulo\
                    \ the given value.\n\
                    \\tclose - If there is a context open closes it \n\
                    \ Examples: \n\n\
                    \ \tpoly> def x := True\n\
                    \ \tsaved x : Bool\n\
                    \ \tpoly> x\n\
                    \ \tTrue\n\
                    \ \tpoly> var X\n\
                    \ \tX is now a polinomial variable\n\
                    \ \tpoly> open 3\n\
                    \ \tContext is now open with : 3\n\
                    \ \tpoly> 3 + 2\n\
                    \ \t2\n\
                    \ \tpoly> close\n\
                    \ \tSuccessfully closed the context"
    Numbers     -> "Number represent complex number.\
                    \They are declared and used in the usual way:\
                    \ Examples: \n\n\
                    \ \tpoly> 3 * 2\n\
                    \ \t6\n\
                    \ \tpoly> 3 + 2*i\n\
                    \ \t3+2i\n\
                    \ \tpoly> 142 / 14\n\
                    \ \t71/7\n"
    Bools       -> "Bools are used in the usual ways:\
                    \ Examples: \n\n\
                    \ \tpoly> True | False\n\
                    \ \tTrue\n\
                    \ \tpoly> True & False\n\
                    \ \tFalse\n\
                    \ \tpoly> if True then 32 else 2\n\
                    \ \t32\n"
--    _           -> "No such topic, run `docs topic` to see avaliable topics\n"
    where
        alltopicsPretty = foldMap ((++) "\n\t\t" . show) alltopics

        alltopics :: [Topic]
        alltopics =  enumFrom (toEnum 0)

read_ :: IO String
read_ = do
    putStr "poly> "
    hFlush stdout
    getLine

-- This typechecks
handle' :: Either TTm TopDef -> ExceptT String GState String
handle' tm_def = case tm_def of
        Left tm   -> show <$> runTypedTerm tm
        Right def -> handleTopDef def

handleError' :: ExceptT String GState String -> StateT GEnv IO String
handleError' = mapStateT (return . runIdentity) . (either id id <$>) . runExceptT

evalFile :: String -> String -> GStateT IO String
evalFile filename cs = case parseStringFile filename $ T.pack cs of
    Left a -> return $ errorBundlePretty a
    Right tms_defs -> handleError' $ unlines <$> mapM handle' tms_defs

evalRepl :: String -> GStateT IO String
evalRepl cs = case parseStringRepl $ T.pack cs of
    Left a   -> return $ errorBundlePretty a
    Right tm_co_def_b -> 
        case tm_co_def_b of
            Left _           -> return "" -- it was an empty line
            Right tm_def_com -> 
                case tm_def_com of
                    OLeft tm    -> handleErrorShow (runTypedTerm tm)
                    OMiddle co  -> handleCommand co
                    ORight def  -> handleErrorString (handleTopDef def)

handleErrorShow :: Show a => ErrorT GState a -> GStateT IO String
handleErrorShow = mapStateT (return . first (eitherIdL show) . runIdentity) . runExceptT

handleErrorString :: ErrorT GState String -> GStateT IO String
handleErrorString = mapStateT (return . first eitherId . runIdentity) . runExceptT

handleTopDef :: TopDef -> ErrorT GState String
handleTopDef = \case
    LetDef name ttm -> do
        (t , val) <- typeCheckAndEval ttm
        modify $ insertType (name, t)
        modify $ insertVal (name, val)
        return ("saved " ++ T.unpack name ++ " : " ++ show t)
    VarDef name     -> do
        case stringToComplexPoly $ T.unpack name of
            Just p  -> do
                modify $ insertType (name, TCPoly)
                modify $ insertVal (name, VCPoly p)
                return $ T.unpack name ++ " is now a polinomial variable"
            Nothing -> throwError (T.unpack name ++ " cannot be made a polinomial variable")
    -- We check if a is a valid VCNum or VCPoly
    OpenDef ttm -> do
        b <- lift isContextOpen
        if b
            then
                throwError "Error: Context already open, close it with `close`"
            else do
                (_, val) <- typeCheckAndEval ttm
                case val of
                    VCNum  n -> do
                        modify $ setZmodN $ Just n
                        return $ "Context is now open with : " ++ show n
                    VCPoly p -> do
                        modify $ setZmodF $ Just p 
                        return $ "Context is now open with : " ++ show p
                    _        -> throwError "Error : Not a valid number/polinomial"
    -- We clear out both ZmodN and ZmodF
    CloseDef -> do
        b <- lift isContextOpen
        if not b
            then
                throwError "Error: Context already closed"
            else do
                modify $ setZmodN Nothing
                modify $ setZmodF Nothing
                return "Successfully closed the context"


handleCommand :: Command -> GStateT IO String
handleCommand = \case
    PrintHelp   -> return help
    -- Run tm and print out the time it took
    RunTimed tm -> do  
        t1 <- lift $ getTime Monotonic
        rs <- handleErrorShow (runTypedTerm tm)
        t2 <- lift $ getTime Monotonic
        return $ rs ++ "\nrunning it took: " ++ show (toNanoSecs $ diffTimeSpec t1 t2) ++ " ns"
    LoadFile ns -> do
        modify $ modifyFiles (union (map T.unpack ns)) -- we add the files the user wanted to load 
        unlines <$> loadFiles (map T.unpack ns) -- load files 
    GetType  tm -> handleErrorShow (typeCheck [] tm)  -- we have to typecheck tm then print out the type
    GetInfo  tp -> return $ helpOnTopic tp
    ReloadFiles -> do
        env <- get
        unlines <$> loadFiles (getFiles env) -- try to reload all files in GEnv'a files field

print_ :: String -> IO ()
print_ = putStrLn

runRepl :: GEnv -> IO ()
runRepl = evalStateT (do
    [logs] <- loadFiles ["..\\Prelude.poly"]
    lift $ putStrLn logs
    runStatefulRepl)

runStatefulRepl :: GStateT IO ()
runStatefulRepl = do
    inp <- lift read_                   -- Lift IO into StateT IO
    unless (trimS inp == ":q") $ do
        tm <- evalRepl inp
        lift $ print_ tm                -- Lift IO into StateT IO
        runStatefulRepl
    where
        trimS = T.unpack . T.strip . T.pack