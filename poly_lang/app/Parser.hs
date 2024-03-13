{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Core

import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Void
import Data.Functor
import Text.Megaparsec
import Text.Megaparsec.Debug (dbg)
import Control.Monad.Combinators.Expr
import Data.Text hiding (elem, empty)

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

-- TODO: Change it to Text
type Parser = Parsec Void Text

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme ws

symbol :: Text -> Parser Text
symbol s = lexeme (C.string s)

char :: Char -> Parser Char
char c = lexeme (C.char c)

parens :: Parser a -> Parser a
parens p   = char '(' *> p <* char ')'

keywords :: [Name]
keywords = ["\\", "let", "mod", "div"]

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
pLit = pInt <|> pBool

pInt :: Parser TTm
pInt = do
    i <- lexeme L.decimal
    return $ TLit $ LNumber i

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
      prefix  "-"   TNeg
    ] ,
    [
      binaryL "*"   (TBinOp Times)
    , binaryL "/"   (TBinOp Div)
    , binaryL "div" (TBinOp IntDiv)
    , binaryL "mod" (TBinOp Mod)
    , binaryL "="   (TBinOp Eq)
    ] ,
    [ 
      binaryL "+"   (TBinOp Plus)
    , binaryL "-"   (TBinOp Minus)
    , binaryL "|"   (TBinOp Or)
    , binaryL "&"   (TBinOp And)
    , binaryL "^"   (TBinOp Pow)
    , binaryL "<="  (TBinOp Lte)
    , binaryL ">="  (TBinOp Gte)
    , binaryL "<"   (TBinOp Lt)
    , binaryL ">"   (TBinOp Gt)
    ]
  ]

binaryL :: Text -> (a -> a -> a) -> Operator Parser a
binaryL n f = InfixL (f <$ symbol n)

binaryR :: Text -> (a -> a -> a) -> Operator Parser a
binaryR n f = InfixR (f <$ symbol n)

prefix, postfix :: Text -> (a -> a) -> Operator Parser a
prefix  n f = Prefix  (f <$ symbol n)
postfix n f = Postfix (f <$ symbol n)

-- A Expr is a tree with nodes (+), (*), ... and leafes pLit, or pVariable
pExprT :: Parser TTm
pExprT = try $ choice 
    [ 
      {-dbg "parens expr" -} (parens pExpr) <?> "parens expr"
    , {-dbg "literal"     -} pLit           <?> "literal"
    , {-dbg "variable"    -} pVariable      <?> "variable"
    ]

pExpr :: Parser TTm
pExpr = makeExprParser pExprT operatorTable

pBind :: Parser Text
pBind  = pIdent <|> symbol "_"

-- TODO : Lam could be a prefix operator
--pLam :: Parser TTm
pLam :: Parser TTm
pLam = do
    void $ char '\\'
    x <- pBind
    void $ char ':'
    t <- pType
    void $ symbol "."
    u <- pTm
    return $ TLam x t u


-- TODO : Let could also be a prefix operator
pLet :: Parser TTm
pLet = do
    pKeyword "let"
    x <- pBind
    void $ symbol ":="
    t <- pTm
    void $ symbol ";"
    u <- optional pTm
    return $ case u of
        Just u' -> TLet x t u'
        Nothing -> TLet x t $ TLit LTop

pBaseType :: Parser Type
pBaseType = choice
    [ symbol "Num"  $> TNumber
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
    , {-dbg "letbind"-} parens pTm
    ] <?> "a valid term")

pSrc :: Parser TTm
pSrc = ws *> pTm <* eof

parseString :: Text -> Either (ParseErrorBundle Text Void) TTm
parseString src = parse pSrc "(stdin)" src