{-# LANGUAGE LambdaCase, PatternSynonyms #-}

module Core.AST where

import Ring
import Data.Text hiding (unwords)

type BoolOpType = Bool -> Bool -> Bool
type NumOpType = Number -> Number -> Number
type PolyOpType = CPolyMulti -> CPolyMulti -> CPolyMulti
type OpType = forall a . (Num a, Fractional a, Integral a) => a -> a -> a

type Name = Text

data TTm
    = TVar    Name
    | TLam    Name Type TTm
    | TApp    TTm  TTm
    | TLet    Name TTm  TTm
    | TIfThenElse TTm TTm TTm
    | TLit    Literal
    | TBinOpPoly BinOp PolyOpType TTm TTm
    | TBinOpBool BinOp BoolOpType TTm TTm
    | TBinOpNum  BinOp NumOpType TTm TTm
    | TBinOp  BinOp TTm TTm
    | TPrefix PrefixOp TTm
--    deriving Show

instance Show TTm where
    show t = show (loseType t)

loseType :: TTm -> Tm
loseType = \case
    TVar n        -> Var n
    TLam n _ e    -> Lam n (loseType e)
    TApp t u      -> App (loseType t) (loseType u)
    TLet n t u    -> Let n (loseType t) (loseType u)
    TLit l        -> Lit l
    TPrefix op t  -> Prefix op (loseType t)
    TBinOp op t u -> BinOp op (loseType t) (loseType u)
    TIfThenElse b t u -> IfThenElse (loseType b) (loseType t) (loseType u)
    TBinOpPoly op f t u -> BinOpPoly op f (loseType t) (loseType u)
    TBinOpBool op f t u -> BinOpBool op f (loseType t) (loseType u)
    TBinOpNum op f t u  -> BinOpNum op f (loseType t) (loseType u)

data PrefixOp
    = Neg
    | Factor
    | Irred
    | Der

instance Show PrefixOp where
    show = \case
        Neg    -> "-"
        Factor -> "factor"
        Irred  -> "irred"
        Der    -> "derivative"

-- Abs
data BinOp
    = And
    | Or
    | Eq
    | Plus
    | Times
    | Minus
    | Div
    | IntDiv
    | Mod
    | Pow
    | Lte
    | Gte
    | Lt
    | Gt
    deriving Eq

instance Show BinOp where
    show = \case
        And     -> "&"
        Or      -> "|"
        Eq      -> "="
        Plus    -> "+"
        Times   -> "*"
        Minus   -> "-"
        Div     -> "/"
        IntDiv  -> "div"
        Mod     -> "mod"
        Pow     -> "^"
        Lte     -> "<="
        Gte     -> ">="
        Lt      -> "<"
        Gt      -> ">"

data Type
    = TArr Type Type
    | TNumber
    | TBool
    | TPoly
    | TTop
    deriving (Eq)

instance Show Type where
    show = \case
        TArr t u -> unwords [show t, " -> " ,show u]
        TNumber  -> "Num"
        TBool    -> "Bool"
        TPoly    -> "Poly"
        TTop     -> "Top"

data Tm
    = Var Name
    | Lam Name Tm
    | App Tm Tm
    | Let Name Tm Tm
    | IfThenElse Tm Tm Tm
    | Lit Literal
    | Prefix PrefixOp Tm
    | BinOp BinOp Tm Tm
    | BinOpPoly BinOp PolyOpType Tm Tm
    | BinOpBool BinOp BoolOpType Tm Tm
    | BinOpNum  BinOp NumOpType Tm Tm

showTm :: Tm -> String
showTm = \case
    Var n              -> unpack n
    App t u            -> unwords ["(",showTm t,showTm u,")"]
    Lam n t            -> unwords ["(\\",unpack n,"->",showTm t,")"]
    Let n t u          -> unwords ["(let",unpack n,"=",showTm t,showTm u,")"]
    IfThenElse b t u   -> unwords [showTm b,showTm t,showTm u]
    Lit (LBool l)      -> show l
    Lit (LNumber l)    -> show l
    Lit (LPoly l)      -> show l
    Lit LTop           -> "tt"
    Prefix op t        -> unwords [show op ,showTm t]
    BinOp op t u       -> unwords [showTm t,show op,showTm u]
    BinOpPoly op _ t u -> unwords [showTm t,show op,showTm u]
    BinOpBool op _ t u -> unwords [showTm t,show op,showTm u]
    BinOpNum  op _ t u -> unwords [showTm t,show op,showTm u]

instance Show Tm where
    show = showTm

data Literal
    = LNumber Number
    | LBool Bool
    | LPoly CPolyMulti
    | LTop
    deriving Show

data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Val)
    | VLit Literal
    | VPrefix PrefixOp Val
    | VBinOp BinOp Val Val
    | VBinOpPoly BinOp PolyOpType Val Val
    | VBinOpBool BinOp BoolOpType Val Val
    | VBinOpNum  BinOp NumOpType Val Val

pattern VBool :: Bool -> Val
pattern VBool b = VLit (LBool b)

pattern VNumber :: Number -> Val
pattern VNumber n = VLit (LNumber n)

pattern VPoly :: CPolyMulti -> Val
pattern VPoly n = VLit (LPoly n)