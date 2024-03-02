module Parser where


{-
| So far this is Ctrl+c Ctrl+v
| TODO :
        impl polinome parsing and more unary and binary operators
        clean up, maybe find easier way to do it
        check if its correct
        waaay laaater : lose the type parseing
-}

import Core
import Control.Applicative hiding (many, some)
import Control.Monad
import Data.Char
import Data.Void
import Data.Functor
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
keyword x = x == "f" || x == "\\" || x == "in" || x == "let"

-- TODO : this is magic i should ask what it does
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

pAtom :: Parser TTm --try might be needed here, or choice
pAtom  = pLit <|> (TVar <$> pIdent) <|> parens pTm

pSpine :: Parser TTm
pSpine = foldl1 TApp <$> some pAtom


-- should work like : 
-- <type> ::= "Int" | "Bool" | <type> "->" <type>
-- type = pTypeLit optional ("->" type)
-- pTypeLit = "Int" | "Bool" | "(" type ")"
--

pType :: Parser Type
pType =  do 
    t <- pBaseType
    t' <- optional $ do 
        void $ symbol "->"
        pType
    case t' of
        Just a  -> return $ TArr t a
        Nothing -> return t

pBaseType :: Parser Type
pBaseType = choice
    [ symbol "Int"  $> TInt
    , symbol "Bool" $> TBool
    , parens pType
    ]

--pType :: Parser Type
--pType  = pArr <|> (symbol "Int" *> return TInt) <|> (symbol "Bool" *> return TBool)

{-
pArr :: Parser Type
pArr = do
    void $ char '('
    a <- pType
    void $ symbol "->"
    b <- pType
    void $ char ')'
    return $ TArr a b
-}

pBinAll :: Parser TTm
pBinAll = choice $ map (try . pBinOp) ['+','*','|','&']

pLit :: Parser TTm
pLit = pInt <|> pBool

pInt :: Parser TTm
pInt = do
    i <- lexeme L.decimal
    return $ TLit $ LInt i

pBool :: Parser TTm
pBool = choice
        [ TLit (LBool True)  <$ symbol "true"
        , TLit (LBool False) <$ symbol "false"]

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
    return $ h op a b
    where
        h '+' = TPlus
        h '*' = TTimes
        h '&' = TAnd
        h '|' = TOr
        h _   = error "Unsupported operator "
    -- Throw errors

pLam :: Parser TTm
pLam = do
    void $ char 'f' <|> char '\\'
    x <- pBind  --some pBind
    void $ char ':'
    t <- pType
    void $ symbol "."
    u <- pTm
    return $ TLam x t u  --foldr Lam t xs

pLet :: Parser TTm
pLet = do
    pKeyword "let"
    x <- pBind
    void $ symbol "="
    t <- pTm
    void $ symbol ";"
    u <- optional pTm
    return $ case u of
        Just u' -> TLet x t u'
        Nothing -> TLet x t $ TLit LTop
--    return $ TLet x t u

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

ptest1 = parseString "let intId = (f x : Int -> x); (intId 3)"

mTm :: String
mTm = "f y : (Bool -> Int) . f x : ((Bool -> Int) -> (Int -> Bool)) . x y"