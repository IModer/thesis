{-# LANGUAGE LambdaCase, PatternSynonyms #-}

module Core.AST where

--import Ring
import Core.Types
import Data.Euclidean
import Data.Semiring
import Data.Text hiding (unwords)

type PredType = forall a . Ord a => a -> a -> Bool

type FieldOp = forall a . (Field a) => a -> a -> a -- Num and CNum
type EuclideanOp = forall a . (Euclidean a, Ring a) => a -> a -> a -- Poly and CPoly
type BoolOp = Bool -> Bool -> Bool

--type OpType = forall a . (Num a, Fractional a, Integral a) => a -> a -> a
--type NumOpType = Number -> Number -> Number
--type PolyOpType = CPolyMulti -> CPolyMulti -> CPolyMulti

type Name = Text

data TTm
    = TVar    Name
    | TLam    Name Type TTm
    | TApp    TTm  TTm
    | TLet    Name TTm  TTm
    | TIfThenElse TTm TTm TTm
    | TLit    Literal
    | TBinPred   BinOp PredType TTm TTm
    | TBinOpBool BinOp BoolOp TTm TTm
    | TBinFieldOp  BinOp FieldOp TTm TTm
    | TBinEucOp BinOp EuclideanOp TTm TTm
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
    TIfThenElse b t u -> IfThenElse (loseType b) (loseType t) (loseType u)
    TPrefix     op t  -> Prefix op (loseType t)
    TBinPred    op f t u -> BinPred op f (loseType t) (loseType u) 
    TBinOpBool  op f t u -> BinOpBool op f (loseType t) (loseType u)
    TBinFieldOp   op f t u -> BinFieldOp op f (loseType t) (loseType u)
    TBinEucOp  op f t u -> BinEucOp op f (loseType t) (loseType u)

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
    | Plus
    | Times
    | Minus
    | Div
    | IntDiv
    | Mod
    | Pow
    | Eq
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
    | TNum
    | TCNum
    | TPoly
    | TCPoly
    | TBool
    | TTop
    deriving (Eq)

instance Show Type where
    show = \case
        TArr t u -> unwords [show t, " -> " ,show u]
        TCNum    -> "Complex Number"
        TNum     -> "Number"
        TBool    -> "Bool"
        TCPoly   -> "Complex Poly"
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
    | BinPred BinOp PredType Tm Tm
    | BinOpBool BinOp BoolOp Tm Tm
    | BinFieldOp  BinOp FieldOp Tm Tm
    | BinEucOp BinOp EuclideanOp Tm Tm
--    | BinOp BinOp OpType Tm Tm

showTm :: Tm -> String
showTm = \case
    Var n              -> unpack n
    App t u            -> unwords ["(",showTm t,showTm u,")"]
    Lam n t            -> unwords ["(\\",unpack n,"->",showTm t,")"]
    Let n t u          -> unwords ["(let",unpack n,"=",showTm t,showTm u,")"]
    IfThenElse b t u   -> unwords [showTm b,showTm t,showTm u]
    Lit (LBool l)      -> show l
    Lit (LNum l)       -> show l
    Lit (LCNum l)      -> show l
    Lit (LPoly l)      -> show l
    Lit (LCPoly l)     -> show l
    Lit (LTop _)       -> "tt"
    Prefix op t        -> unwords [show op ,showTm t]
    BinPred op _ t u   -> unwords [showTm t,show op,showTm u]
    BinOpBool op _ t u -> unwords [showTm t,show op,showTm u]
    BinEucOp op _ t u -> unwords [showTm t,show op,showTm u]
    BinFieldOp  op _ t u -> unwords [showTm t,show op,showTm u]

instance Show Tm where
    show = showTm

data Literal
    = LNum Frac
    | LCNum (Complex Frac)
    | LPoly (PolyMulti Frac)
    | LCPoly (PolyMulti (Complex Frac))
    | LBool Bool
    | LTop ()
--    deriving Show

data Val
    = VVar Name
    | VApp Val Val
    | VLam Name (Val -> Val)
    | VLit Literal
    | VPrefix PrefixOp Val
    | VBinPred BinOp PredType Val Val
    | VBinOpBool BinOp BoolOp Val Val
--    | VBinOp BinOp Val Val
    | VBinEucOp BinOp EuclideanOp Val Val
    | VBinFieldOp  BinOp FieldOp Val Val

pattern VBool :: Bool -> Val
pattern VBool b = VLit (LBool b)

pattern VNum :: Frac -> Val
pattern VNum n = VLit (LNum n)

pattern VCNum :: Complex Frac -> Val
pattern VCNum n = VLit (LCNum n)

pattern VPoly :: PolyMulti Frac -> Val
pattern VPoly n = VLit (LPoly n)

pattern VCPoly :: PolyMulti (Complex Frac) -> Val
pattern VCPoly n = VLit (LCPoly n)