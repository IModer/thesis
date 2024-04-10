{-# LANGUAGE LambdaCase, PatternSynonyms #-}

module Core.AST where

--import Ring
import Core.Types
import Data.Euclidean
import Data.Semiring
import Data.Text hiding (unwords)
import Control.Monad.State
import Control.Monad.Except
--import Ring

type PredType = forall a . Ord a => a -> a -> Bool

type FieldOp = forall a . (Field a) => a -> a -> a -- Num and CNum
type EuclideanOp = forall a . (Euclidean a, Ring a) => a -> a -> a -- Poly and CPoly
type RingOp = forall a . (Ring a) => a -> a -> a
type BoolOp = Bool -> Bool -> Bool

type Name = Text

type TEnv = [(Name, Type)]
type VEnv  = [(Name, Val)]

--GEnv
data GEnv = GEnv { typeEnv :: TEnv
                 , nameEnv :: VEnv
                 , zmodn :: Maybe Frac
                 , zmodf :: Maybe (PolyMulti Frac) }


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
    | TBinRingOp BinOp RingOp TTm TTm
    | TPrefix PrefixOp TTm

data PrefixOp
    = Neg
    | Factor
    | Irred

instance Show PrefixOp where
    show = \case
        Neg    -> "-"
        Factor -> "factor"
        Irred  -> "irred"

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
--    | TNum
--    | TPoly
    | TCNum
    | TCPoly
    | TBool
    | TTop
    deriving (Eq)

instance Show Type where
    show = \case
        TArr t u -> unwords ["(",show t,"->",show u,")"]
        TCNum    -> "CNum"
--        TNum     -> "Number"
        TBool    -> "Bool"
        TCPoly   -> "CPoly"
--        TPoly    -> "Poly"
        TTop     -> "Top"

showTTm :: TTm -> String
showTTm = \case
    TVar n              -> unpack n
    TApp t u            -> unwords ["(",showTTm t,showTTm u,")"]
    TLam n t e          -> unwords ["(\\",unpack n,":",show t,".",showTTm e,")"]
    TLet n t u          -> unwords ["(let",unpack n,"=",showTTm t,showTTm u,")"]
    TIfThenElse b t u   -> unwords ["if",showTTm b,"then",showTTm t,"else",showTTm u]
    TLit (LBool l)      -> show l
    TLit (LCNum l)      -> show l
    TLit (LCPoly l)     -> show l
    TLit (LTop _)       -> "tt"
    TPrefix op t        -> unwords [show op ,showTTm t]
    TBinPred op _ t u   -> unwords [showTTm t,show op,showTTm u]
    TBinOpBool op _ t u -> unwords [showTTm t,show op,showTTm u]
    TBinEucOp op _ t u  -> unwords [showTTm t,show op,showTTm u]
    TBinRingOp op _ t u -> unwords [showTTm t,show op,showTTm u]
    TBinFieldOp  op _ t u -> unwords [showTTm t,show op,showTTm u]

instance Show TTm where
    show = showTTm

data Literal
    = LCNum (Complex Frac)
--    | LNum Frac
--    | --LPoly (PolyMulti Frac)
    | LCPoly (PolyMulti (Complex Frac))
    | LBool Bool
    | LTop ()

data Val
    = VVar Name
    | VApp Val Val
--    | VLam Name Type (Val -> Val)
    | VLam Name Type (Val -> ExceptT String (State GEnv) Val)
    | VLit Literal
    | VIfThenElse Val TTm TTm
    | VPrefix PrefixOp Val
    | VBinPred BinOp PredType Val Val
    | VBinOpBool BinOp BoolOp Val Val
    | VBinEucOp BinOp EuclideanOp Val Val
    | VBinRingOp BinOp RingOp Val Val
    | VBinFieldOp  BinOp FieldOp Val Val

pattern VBool :: Bool -> Val
pattern VBool b = VLit (LBool b)

{-
pattern VNum :: Frac -> Val
pattern VNum n = VLit (LNum n)

pattern VPoly :: PolyMulti Frac -> Val
pattern VPoly n = VLit (LPoly n)
-}

pattern VCNum :: Complex Frac -> Val
pattern VCNum n = VLit (LCNum n)

pattern VCPoly :: PolyMulti (Complex Frac) -> Val
pattern VCPoly n = VLit (LCPoly n)

pattern VTop :: Val
pattern VTop = VLit (LTop ())