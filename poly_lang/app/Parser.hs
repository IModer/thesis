{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wincomplete-patterns #-}

module Parser
    ( parseStringRepl
    , parseStringFile
    , pCommandLineCommand
    , Command(..)
    , TopDef(..)
    , Topic(..)
    , CommandLineCommand(..) ) where

import Core.AST
import Core.Types
import Data.Euclidean
import Data.Semiring
import Lib (Option(..), eitherOptionP, filterToEither)

import Prelude hiding ((*), (+), negate, (-), quot, rem, lcm, gcd)

import Control.Applicative hiding (many, some)
import Control.Monad (void, guard)
import Data.Char (isAlphaNum)
import Data.Void (Void)
import Data.Functor (($>))
import Text.Megaparsec 
    (Parsec
    , ParseErrorBundle
    , parse
    , many, eof, some, try
    , eitherP, (<?>), choice
    , takeWhile1P, sepBy, between)
--import Text.Megaparsec.Debug (dbg)
import Control.Monad.Combinators.Expr (Operator(..), makeExprParser)
import Data.Text hiding (elem, empty, filter, map, foldr)

import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

------
-- Basic parsers with whitespace
------

type Parser = Parsec Void Text

-- hspace does not accept newlines
ws :: Parser ()
ws = L.space C.hspace1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: Text -> Parser Text
symbol s = lexeme (C.string s)

char :: Char -> Parser Char
char c = lexeme (C.char c)

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

keywords :: [Name]
keywords = ["\\", "let", "in", "def"
            , "mod", "div"
            , "var" 
            ,"if", "then", "else"
            , "open", "close" , "Zmod"
            , "tt", "i", "fix"
            ]

keyword :: Text -> Bool
keyword x = x `elem` keywords

pKeyword :: Text -> Parser ()
pKeyword kw = do
    void $ C.string kw
    (takeWhile1P Nothing isAlphaNum *> empty) <|> ws

pIdent :: Parser Name
pIdent = try $ do
    x <- takeWhile1P Nothing isIdent
    guard (not (keyword x))
    x <$ ws
    where
        isIdent :: Char -> Bool
        isIdent x = isAlphaNum x || (x `elem` ("'_" :: String))

pVariable :: Parser TTm
pVariable = TVar <$> pIdent

pFancyList :: Parser TTm
pFancyList = do
    lst <- between (symbol "[") (symbol "]") (pTm `sepBy` symbol ",")
    return $ foldr TListCons (TLit $ LList []) lst

pLit :: Parser TTm
pLit = try $ choice
    [ pInt <?> "integer literal"
    , pBool <?> "bool literal"
    , pCompI <?> "complex literal"
    , pTT <?> "tt literal"
    , pFancyList <?> "fancy list literal"]

pTT :: Parser TTm
pTT = do
    pKeyword "tt"
    return $ TLit $ LTop ()

pCompI :: Parser TTm
pCompI = do
    pKeyword "i" <?> "i keyword"
    return $ TLit $ LCNum (zero :+ one) -- i

pInt :: Parser TTm
pInt = do
    i <- lexeme L.decimal
    return $ TLit $ LCNum (i %% 1 :+ zero) 

pBool :: Parser TTm
pBool = choice
        [ TLit (LBool True)  <$ symbol "True"
        , TLit (LBool False) <$ symbol "False"]

operatorTable :: [[Operator Parser TTm]]
operatorTable =
  [ 
    [
        binaryL ""  TApp
    ] ,
    [
      prefix  "-"          (TPrefix Neg   )
    , binaryR "::"         (TListCons)
    ] ,
    [
      binaryL "*"   (TBinRingOp Times (*)  )
    , binaryL "/"   (TBinFieldOp Div  ((unsafe .) . divide) )
    , binaryL "div" (TBinEucOp IntDiv quot )
    , binaryL "mod" (TBinEucOp Mod    rem  )
    , binaryL "=="  (TBinPred Eq      (==) )
    ] ,
    [
      binaryL "+"   (TBinRingOp Plus   (+) )
    , binaryL "-"   (TBinRingOp Minus  (-) )
    , binaryL "|"   (TBinOpBool Or    (||) )
    , binaryL "&"   (TBinOpBool And   (&&) )
    , binaryL "<="  (TBinPred Lte (<=) )
    , binaryL ">="  (TBinPred Gte (>=) )
    , binaryL "<"   (TBinPred Lt  (<)  )
    , binaryL ">"   (TBinPred Gt  (>)  )
    ]
  ]

binaryL :: Text -> (a -> a -> a) -> Operator Parser a
binaryL n f = InfixL (f <$ symbol n)

binaryR :: Text -> (a -> a -> a) -> Operator Parser a
binaryR n f = InfixR (f <$ symbol n)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a
prefix  n f = Prefix  (f <$ symbol n)
postfix n f = Postfix (f <$ symbol n)

-- A Expr is a tree with nodes (+), (*), ... and leafs pLit, or pVariable
pExprT :: Parser TTm
pExprT = try $ choice
    [ 
      {-dbg "parens expr" -} (parens pExpr) <?> "parenthesized expression"
    , {-dbg "literal"     -} pLit           <?> "literal"
    , {-dbg "fix"     -}     pFix           <?> "fix expression"
    , {-dbg "variable"    -} pVariable      <?> "variable"
    , {-dbg "function"    -} pLam           <?> "function"
    , {-dbg "let expr"    -} pLet           <?> "let expression"
    , {-dbg "if  expr"    -} pIfThenElse    <?> "if_then_else expression"
    ]

pExpr :: Parser TTm
pExpr = makeExprParser pExprT operatorTable

pBind :: Parser Text
pBind  = pIdent <|> symbol "_"

pFix :: Parser TTm
pFix = do
    pKeyword "fix"
    t <- pTm
    return $ TFix t

pLamArg :: Parser (TTm -> TTm)
pLamArg = do
    xs <- some pBind
    void $ char ':'
    t <- pType
    let xs' = foldr (.) (id) $ map (\x -> TLam x t) xs
    return xs'

pLam :: Parser TTm
pLam = do
    void $ char '\\'
    args <- pLamArg `sepBy` char ','
    void $ char '.'
    u <- pTm
    return $ foldr ($) u args

pIfThenElse :: Parser TTm
pIfThenElse = do
    pKeyword "if"
    b <- pTm
    pKeyword "then"
    e1 <- pTm
    pKeyword "else"
    e2 <- pTm
    return $ TIfThenElse b e1 e2

pLet :: Parser TTm
pLet = do
    pKeyword "let"
    x <- pBind
    ns <- optional $ parens (pLamArg `sepBy` char ',')
    pKeyword ":="
    t <- pTm
    pKeyword "in"
    u <- pTm
    case ns of
        Just ns' -> return $ TLet x (foldr ($) t ns') u
        Nothing -> return $ TLet x t u

pBaseType :: Parser Type
pBaseType = choice
    [ symbol "Num"  $> TCNum
    , symbol "Poly" $> TCPoly
    , symbol "Bool" $> TBool
    , symbol "Top"  $> TTop
    , symbol "List" $> TList
    , parens pType
    ]

operatorTableT :: [[Operator Parser Type]]
operatorTableT = [[binaryR "->" TArr]]

pType :: Parser Type
pType = makeExprParser pBaseType operatorTableT

pTm :: Parser TTm
pTm  = try (choice
    [ {-dbg "expr"    -} pExpr
    , {-dbg "lambda"  -} pLam
    , {-dbg "letbind" -} pLet
    , {-dbg "fix"     -} pFix
    , {-dbg "if bind" -} pIfThenElse
    , {-dbg "parens " -} parens pTm
    ] <?> "a valid term")

data CommandLineCommand
    = PrintHelpCL
    | NoSuchCommandCl
    | GetInfoCL Topic
    | LoadFileCL [String]

pCommandLineCommand :: [String] -> CommandLineCommand
pCommandLineCommand ("load":files) = LoadFileCL files
pCommandLineCommand ["help"]       = PrintHelpCL
pCommandLineCommand ["docs",topic] = 
    let e_t = parse pInfoTopic "(cmd)" $ pack topic in
        case e_t of
            Left _ -> NoSuchCommandCl
            Right tpc -> GetInfoCL tpc
pCommandLineCommand _                = NoSuchCommandCl

data Command
    = PrintHelp         -- :h              | : help
    | RunTimed TTm      -- :b <tm>         | :timeit <tm>  -- Bentchmark
    | LoadFile [Name]   -- :l <f> <f2> ... | :load <f> <f2> ...
    | GetType  TTm      -- :t <tm>         | :type <tm>
    | GetInfo  Topic    -- :i <topic>      | :info <topic>
    | ReloadFiles       -- :r              | :reload
    deriving (Show)

data TopDef
    = LetDef Name TTm   -- def x := e
    | VarDef Name       -- var X
    | OpenDef TTm       -- open t
    | CloseDef          -- close
    deriving (Show)

pLetDef :: Parser TopDef
pLetDef = do
    pKeyword "def"
    x <- pBind
    ns <- optional $ parens (pLamArg `sepBy` char ',')
    void $ symbol ":="
    t <- pTm
    case ns of
        Just ns' -> return $ LetDef x (foldr ($) t ns')
        Nothing -> return $ LetDef x t

pVarDef :: Parser TopDef
pVarDef = do
    void $ symbol "var"
    i <- pIdent
    return $ VarDef i

pOpenDef :: Parser TopDef
pOpenDef = do
    void $ symbol "open"
    tm <- pTm
    return $ OpenDef tm

pCloseDef :: Parser TopDef
pCloseDef = do
    void $ symbol "close"
    return CloseDef

pTopDef :: Parser TopDef
pTopDef = choice [pLetDef, pVarDef, pOpenDef, pCloseDef] <?> "a valid top level definition"

pSimpleCommand :: Char -> Text -> Command -> Parser Command
pSimpleCommand c s co = do
    choice [void $ C.string s, void $ C.char c]
    return co

pFileName :: Parser Text
pFileName = do
    x <- takeWhile1P (Just "a file name") isFileNameChar
    x <$ ws

isFileNameChar :: Char -> Bool
isFileNameChar c = isAlphaNum c || c `elem` chars
    where
        chars :: String
        chars = "\\.:"

data Topic
    = MetaTopic
    | Polinomials
    | Lists
    | Functions
    | Commands
    | TopDefs
    | Numbers
    | Bools
    | Builtins
    deriving (Show, Eq, Enum, Bounded)

pInfoTopic :: Parser Topic
pInfoTopic = choice [ MetaTopic   <$ symbol "ListTopics"
                    , Polinomials <$ symbol "Polinomials"
                    , Lists       <$ symbol "Lists"
                    , Functions   <$ symbol "Functions"
                    , Commands    <$ symbol "Commands"
                    , TopDefs     <$ symbol "TopDefs"
                    , Numbers     <$ symbol "Numbers"
                    , Builtins    <$ symbol "Builtins"
                    ] <?> "a valid topic, to see all topics run `:i ListTopics`"

pLoadFileCommand :: Parser Command
pLoadFileCommand = do
    choice [void $ symbol "load", void $ char 'l']
    filenames <- many pFileName
    return $ LoadFile filenames

pGetInfoCommand :: Parser Command
pGetInfoCommand = do
    choice [void $ symbol "info", void $ char 'i']
    infotopic <- pInfoTopic
    return $ GetInfo infotopic

pGetTypeCommand :: Parser Command
pGetTypeCommand = do
    choice [void $ symbol "type", void $ char 't']
    tm <- pTm
    return $ GetType tm

pRunTimedCommand :: Parser Command
pRunTimedCommand = do
    choice [void $ symbol "timeit", void $ char 'b']
    tm <- pTm
    return $ RunTimed tm

pReloadFilesCommand :: Parser Command
pReloadFilesCommand = do
    choice [void $ symbol "reload", void $ char 'r']
    return ReloadFiles

pCommand :: Parser Command
pCommand = do
    void (C.char ':') <?> "a valid command"
    choice
        [ pSimpleCommand 'h' "help" PrintHelp   <?> "a help command"
        , pRunTimedCommand                      <?> "benchmark commad"
        , pGetTypeCommand                       <?> "types command"
        , pGetInfoCommand                       <?> "info command"
        , pLoadFileCommand                      <?> "load file command"
        , pReloadFilesCommand                   <?> "reload file command"
        ]

pReplLine :: Parser (Either () (Option TTm Command TopDef))
pReplLine = ws *> eitherP eof (try $ eitherOptionP pTm pCommand pTopDef)

pRepl :: Parser (Either () (Option TTm Command TopDef))
pRepl = do
    tm_com_def_b <- pReplLine
    void eof
    return tm_com_def_b

pNewLine :: Parser ()
pNewLine = void $ ws *> C.newline

-- Note : A newline is always needed for end of file
pFileLine :: Parser (Option TTm TopDef ())
pFileLine = do
    tm_def <- eitherOptionP pTm pTopDef pNewLine
    return tm_def

pFile :: Parser [Either TTm TopDef]
pFile = do
    tms_defs <- many pFileLine
    void eof
    return $ filterToEither tms_defs

type ParserErrorT = Either (ParseErrorBundle Text Void)

type ParserOutput = ParserErrorT (Either () (Option TTm Command TopDef) )

parseStringFile :: String -> Text -> ParserErrorT [Either TTm TopDef]
parseStringFile filename = parse pFile $ "(" ++ filename ++")"

parseStringRepl :: Text -> ParserOutput
parseStringRepl = parse pRepl "(poly)"