module Parser where


{-
| So far this is Ctrl+c Ctrl+v
-}

import Core
import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Debug (dbg)

import qualified Text.Megaparsec.Char       as C
import qualified Text.Megaparsec.Char.Lexer as L

-- parsing
--------------------------------------------------------------------------------

type Parser = Parsec Void String

ws :: Parser ()
ws = L.space C.space1 (L.skipLineComment "--") (L.skipBlockComment "{-" "-}")

lexeme :: Parser a -> Parser a
lexeme     = L.lexeme ws

symbol :: String -> Parser String
symbol s   = lexeme (C.string s)

char :: Char -> Parser Char
char c     = lexeme (C.char c)

parens :: Parser a -> Parser a
parens p   = char '(' *> p <* char ')'

keyword :: String -> Bool
keyword x = x == "f" || x == "in" || x == "let"

pIdent :: Parser Name
pIdent = try $ do
    x <- takeWhile1P Nothing isAlphaNum
    guard (not (keyword x))
    x <$ ws

pKeyword :: String -> Parser ()
pKeyword kw = do
    void $ C.string kw
    (takeWhile1P Nothing isAlphaNum *> empty) <|> ws

pBind :: Parser String
pBind  = pIdent <|> symbol "_"

pAtom :: Parser TTm
pAtom  = (TVar <$> pIdent) <|> parens pTm

pSpine :: Parser TTm
pSpine = foldl1 TApp <$> some pAtom

pType :: Parser Type
pType  = pArr <|> (symbol "Int" *> return TInt) <|> (symbol "Bool" *> return TBool)

pArr :: Parser Type
pArr = do
    void $ char '('
    a <- pType
    void $ symbol "->"
    b <- pType
    void $ char ')'
    pure $ TArr a b

pBinAll :: Parser TTm
pBinAll = choice $ map (try . pBinOp) ['+','*','|','&']

pLit :: Parser TTm
pLit = pInt <|> pBool

pInt :: Parser TTm
pInt = do
    i <- lexeme L.decimal
    pure $ TLit $ LInt i

pBool :: Parser TTm
pBool = choice
        [ (TLit (LBool True)  <$ (symbol "true"))
        , (TLit (LBool False) <$ (symbol "false"))]

{-
pBool = do
    b <- (symbol "true" <|> symbol "false")
    pure $ TLit $ LBool (if b == "true" then True else False)
-}

pBinAtom :: Parser TTm
pBinAtom = pLit <|> parens pTm

pBinOp :: Char -> Parser TTm
pBinOp op = do
--    void $ char '('
    a <- pBinAtom
    void $ lexeme (C.char op)
    b <- pTm
--    void $ char ')'
    pure $ (h op) a b
    where
        h '+' = TPlus
        h '*' = TTimes
        h '&' = TAnd
        h '|' = TOr
        h _   = error $ "Unsupported operator "
    -- Throw errors

pLam :: Parser TTm
pLam = do
    void $ char 'f' <|> char '\\'
    x <- pBind  --some pBind
    void $ char ':'
    t <- pType
    void $ symbol "->"
    u <- pTm
    pure $ TLam x t u  --foldr Lam t xs

pLet :: Parser TTm
pLet = do
    pKeyword "let"
    x <- pBind
    void $ symbol "="
    t <- pTm
    void $ symbol ";"
    u <- pTm
    pure $ TLet x t u

--pTm :: Parser TTm
--pTm = dbg "literal" pLit <|> pBinAll <|> pLam <|> pLet <|> pSpine


pTm :: Parser TTm
pTm  = choice
    [ {-dbg "bin"    -} pBinAll
    , {-dbg "literal"-} pLit
    , {-dbg "lambda" -} pLam 
    , {-dbg "letbind"-} pLet  
    , {-dbg "app"    -} pSpine ] <?> "a valid term"


pSrc :: Parser TTm
pSrc = ws *> pTm <* eof

parseString :: String -> Either (ParseErrorBundle String Void) TTm
parseString src = parse pSrc "(stdin)" src

{-
parseStringAndPrint :: String -> IO TTm
parseString src =
    case parse pSrc "(stdin)" src of
        Left e -> do
            putStrLn $ errorBundlePretty e
            exitFailure
        Right t ->
            pure t
-}

-- Test

mTm :: String
mTm = "f y : (Bool -> Int) . f x : ((Bool -> Int) -> (Int -> Bool)) . x y"