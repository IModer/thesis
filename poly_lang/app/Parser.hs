{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Core.AST hiding (Prefix)
import Core.Types
import Data.Euclidean
import Data.Semiring
import Lib

import Prelude hiding ((*), (+), negate, (-), quot, rem, lcm, gcd)

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Void
import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Debug (dbg)
import Control.Monad.Combinators.Expr
import Data.Text hiding (elem, empty, filter, map)

import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L


{- AST : 

--  Ennek a parsolása a pIdent ami bármilyen alphaNumerical Text ami nem keyword
type Name = Text

data TTm
--  pLit és pVariable, ezek a leaf-jei egy Expr-nek
    = TVar Name             -|
    | TLit Literal          -|

--  pLam
    | TLam Name Type TTm       -|
--  pLet
    | TLet Name TTm TTm        -|

--  Expr amiket a makeExprParserrel meg lehetne oldani
    | TApp TTm TTm             -|
    | TPlus TTm TTm             |
    | TTimes TTm TTm            |
    | TAnd  TTm TTm             |
    | TOr  TTm TTm             -|
    deriving Show

data Type 
--  pTBase    
    = TBase

--  tExpr
    | TArr Type Type

-}

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
parens p   = char '(' *> p <* char ')'

keywords :: [Name]
keywords = ["\\", "let", "in", "def"
            , "mod", "div", "factor"
            , "irred", "derivative"
            , "var" 
            ,"if", "then", "else"
            , "open", "close" , "Zmod"
            , "i", "tt"]

keyword :: Text -> Bool
keyword x = x `elem` keywords

pKeyword :: Text -> Parser ()
pKeyword kw = do
    void $ C.string kw
    (takeWhile1P Nothing isAlphaNum *> empty) <|> ws

-- TODO : this is magic i should ask what it does
pIdent :: Parser Name
pIdent = try $ do
    x <- takeWhile1P Nothing isAlphaNum
    guard (not (keyword x))
    x <$ ws

pVariable :: Parser TTm
pVariable = TVar <$> pIdent

pLit :: Parser TTm
pLit = pInt <|> pBool <|> pCompI <|> pTT

pTT :: Parser TTm
pTT = do
    void $ symbol "tt"
    return $ TLit $ LTop ()

pCompI :: Parser TTm
pCompI = do
    void $ symbol "i"
    return $ TLit $ LCNum $ ((0 %% 1) :+ (1 %% 1)) -- i

pInt :: Parser TTm
pInt = do
    i <- lexeme L.decimal
    return $ TLit $ LNum $ i %% 1

pBool :: Parser TTm
pBool = choice
        [ TLit (LBool True)  <$ symbol "true"
        , TLit (LBool False) <$ symbol "false"]

operatorTable :: [[Operator Parser TTm]]
operatorTable =
  [ 
    [
        binaryL ""  TApp
    ] ,
    [
      prefix  "-"          (TPrefix Neg   )
    , prefix  "factor"     (TPrefix Factor)
    , prefix  "irred"      (TPrefix Irred )
    , prefix  "derivative" (TPrefix Der   )
    ] ,
    [
      binaryL "*"   (TBinEucOp Times  (*)  )
    , binaryL "/"   (TBinFieldOp Div  ((unsafe .) . divide)  )
    , binaryL "div" (TBinEucOp IntDiv quot )
    , binaryL "mod" (TBinEucOp Mod    rem  )
    , binaryL "=="  (TBinPred Eq      (==) )
    ] ,
    [
      binaryL "+"   (TBinEucOp Plus   (+)  )
    , binaryL "-"   (TBinEucOp Minus  (-)  )
    , binaryL "|"   (TBinOpBool Or    (||) )
    , binaryL "&"   (TBinOpBool And   (&&) )
--    , binaryL "^"   (TBinOp Pow   (^)  )
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
      {-dbg "parens expr" -} parens pExpr <?> "parens expr"
    , {-dbg "literal"     -} pLit         <?> "literal"
    , {-dbg "variable"    -} pVariable    <?> "variable"
    , {-dbg "function"    -} pLam         <?> "function"
    , {-dbg "let expr"    -} pLet         <?> "let expr"
    , {-dbg "if  expr"    -} pIfThenElse  <?> "if expr"
    ]

pExpr :: Parser TTm
pExpr = makeExprParser pExprT operatorTable

pBind :: Parser Text
pBind  = pIdent <|> symbol "_"

pLam :: Parser TTm
pLam = do
    void $ char '\\'
    x <- pBind
    void $ char ':'
    t <- pType
    void $ symbol "."
    u <- pTm
    return $ TLam x t u

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
    void $ symbol ":="
    t <- pTm
    void $ symbol "in"
    u <- pTm
    return $ TLet x t u

pBaseType :: Parser Type
pBaseType = choice
    [ symbol "Num"  $> TNum
    , symbol "CNum"  $> TCNum
    , symbol "Poly"  $> TPoly
    , symbol "CPoly"  $> TCPoly
    , symbol "Bool" $> TBool
    , symbol "Top"  $> TTop
    , parens pType
    ]

operatorTableT :: [[Operator Parser Type]]
operatorTableT = [[binaryR "->" TArr]]

pType :: Parser Type
pType = makeExprParser pBaseType operatorTableT

pTm :: Parser TTm
pTm  = try (choice
    [ {-dbg "expr"   -} pExpr
    , {-dbg "lambda" -} pLam
    , {-dbg "letbind"-} pLet
    , {-dbg "if bind"-} pIfThenElse
    , {-dbg "parens "-} parens pTm
    ] <?> "a valid term")

data CommandLineCommand
    = PrintHelpCL
    | NoSuchCommandCl
    | GetInfoCL Topic
    | LoadFileCL [String]

pCommandLineCommand :: [String] -> CommandLineCommand
pCommandLineCommand ("load":files)   = LoadFileCL files
pCommandLineCommand ("help":_)       = PrintHelpCL
pCommandLineCommand ("docs":topic:_) = 
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
    deriving Show

data TopDef
    = LetDef Name TTm
    | VarDef Name
    | OpenDef TTm
    | CloseDef
    deriving Show

pLetDef :: Parser TopDef
pLetDef = do
    pKeyword "def"
    x <- pBind
    void $ symbol ":="
    t <- pTm
    --void $ symbol ";"  --?
    return $ LetDef x t

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
pTopDef = choice [pLetDef, pVarDef, pOpenDef, pCloseDef]

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
    | Dummy
    deriving Show

pInfoTopic :: Parser Topic
pInfoTopic = undefined -- TODO

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

pCommand :: Parser Command
pCommand = do
    void $ C.char ':'
    choice
        [ pSimpleCommand 'h' "help" PrintHelp
        , pRunTimedCommand
        , pGetTypeCommand
        , pGetInfoCommand
        , pLoadFileCommand
        ]

pReplLine :: Parser (Option TTm Command TopDef)
pReplLine = ws *> eitherOptionP pTm pCommand pTopDef <* eof

pNewLine :: Parser ()
pNewLine = void $ ws *> C.newline

-- TODO : A newline is always needed for end of file
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

type ParserOutput = ParserErrorT (Option TTm Command TopDef)

parseStringFile :: String -> Text -> ParserErrorT [Either TTm TopDef]
parseStringFile filename = parse pFile $ "(" ++ filename ++")"

parseStringRepl :: Text -> ParserOutput
parseStringRepl = parse pReplLine "(poly)"