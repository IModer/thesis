--{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE LambdaCase, PatternSynonyms #-}

-- Export everything
module Core.AST where

import Core.Types
import Data.Euclidean (rem, Euclidean, Field)
import Data.Semiring (Ring)
import Data.Text hiding (unwords, foldr)
import Control.Monad.State
import Control.Monad.Except
import Prelude hiding (rem)

-- Universally qualified types with constrainst for the AST
type PredType = forall a . Ord a => a -> a -> Bool
type FieldOp = forall a . (Field a) => a -> a -> a
type EuclideanOp = forall a . (Euclidean a, Ring a) => a -> a -> a -- Poly and CPoly
type RingOp = forall a . (Ring a) => a -> a -> a
type BoolOp = Bool -> Bool -> Bool

type Name = Text

type TEnv = [(Name, Type)]
type VEnv  = [(Name, Val)]

--GEnv
data GEnv = GEnv { typeEnv :: TEnv
                 , nameEnv :: VEnv
                 , zmodn :: Maybe (Complex Frac)
                 , zmodf :: Maybe (PolyMulti (Complex Frac))
                 , files :: [String] }

data TTm
    = TVar    Name
    | TLam    Name Type TTm
    | TApp    TTm  TTm
    | TLet    Name TTm  TTm
    | TIfThenElse TTm TTm TTm
    | TLit    Literal
    | TListCons TTm TTm
    | TBinPred   BinOp PredType TTm TTm
    | TBinOpBool BinOp BoolOp TTm TTm
    | TBinFieldOp  BinOp FieldOp TTm TTm
    | TBinEucOp BinOp EuclideanOp TTm TTm
    | TBinRingOp BinOp RingOp TTm TTm
    | TPrefix PrefixOp TTm
    | TFix TTm

data PrefixOp
    = Neg
    deriving (Eq)

instance Show PrefixOp where
    show = \case
        Neg    -> "-"

data BinOp
    = And
    | Or
    | Plus
    | Times
    | Minus
    | Div
    | IntDiv
    | Mod
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
        Lte     -> "<="
        Gte     -> ">="
        Lt      -> "<"
        Gt      -> ">"

data Type
    = TArr Type Type
    | TCNum
    | TCPoly
    | TBool
    | TTop
    | TList
    deriving (Eq)

instance Show Type where
    show = \case
        TArr t u -> unwords ["(",show t,"->",show u,")"]
        TCNum    -> "Num"
        TBool    -> "Bool"
        TCPoly   -> "Poly"
        TTop     -> "Top"
        TList    -> "List"

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
    TLit (LList l)      -> show l
    TListCons t u       -> unwords [showTTm t,"::",showTTm u]
    TPrefix op t        -> unwords [show op ,showTTm t]
    TBinPred op _ t u   -> unwords [showTTm t,show op,showTTm u]
    TBinOpBool op _ t u -> unwords [showTTm t,show op,showTTm u]
    TBinEucOp op _ t u  -> unwords [showTTm t,show op,showTTm u]
    TBinRingOp op _ t u -> unwords [showTTm t,show op,showTTm u]
    TBinFieldOp  op _ t u -> unwords [showTTm t,show op,showTTm u]
    TFix tm             -> unwords ["fix", showTTm tm]

instance Show TTm where
    show = showTTm

data Literal
    = LCNum (Complex Frac)
    | LCPoly (PolyMulti (Complex Frac))
    | LBool Bool
    | LTop ()
    | LList [Val]

instance Eq Literal where
    LCNum c == LCNum c' = c == c'
    LCPoly p == LCPoly p' = p == p'
    LBool b == LBool b' = b == b'
    LTop () == LTop () = True
    -- List doesnt have Eq anyways
    LList _ == LList _ = False
    _ == _ = False    

data Val
    = VVar Name
    | VApp Val Val
    | VLam Name Type (Val -> ExceptT String (State GEnv) Val)
    | VLit Literal
    | VIfThenElse Val TTm TTm
    | VPrefix PrefixOp Val
    | VBinPred BinOp PredType Val Val
    | VBinOpBool BinOp BoolOp Val Val
    | VBinEucOp BinOp EuclideanOp Val Val
    | VBinRingOp BinOp RingOp Val Val
    | VBinFieldOp  BinOp FieldOp Val Val
    | VFix Val

showVal :: Val -> String
showVal = \case
    VVar n              -> unpack n
    VApp t u            -> unwords ["(",showVal t,showVal u,")"]
    VLam n t _          -> unwords ["(\\",unpack n,":",show t,". ___)"] -- we cannot show functions
    VLit (LBool l)      -> show l
    VLit (LCNum l)      -> show l
    VLit (LCPoly l)     -> show l
    VLit (LTop _)       -> "tt"
    VLit (LList l)      -> show l
    VIfThenElse b t u   -> unwords ["if",showVal b,"then",showTTm t,"else",showTTm u]
    VPrefix op t        -> unwords [show op ,showVal t]
    VBinPred op _ t u   -> unwords [showVal t,show op,showVal u]
    VBinOpBool op _ t u -> unwords [showVal t,show op,showVal u]
    VBinEucOp op _ t u  -> unwords [showVal t,show op,showVal u]
    VBinRingOp op _ t u -> unwords [showVal t,show op,showVal u]
    VBinFieldOp  op _ t u -> unwords [showVal t,show op,showVal u]
    VFix u              -> unwords ["fix", showVal u]

instance Show Val where
   show = showVal

pattern VList :: [Val] -> Val
pattern VList l = VLit (LList l)

pattern VBool :: Bool -> Val
pattern VBool b = VLit (LBool b)

infixr 6 ...
infixr 7 ~>

(~>) :: Type -> Type -> Type
(~>) = TArr 

(...) :: Val -> Val -> Val
(...) = VApp

tMod :: TTm -> TTm -> TTm
tMod = TBinEucOp Mod rem

pattern VCNum :: Complex Frac -> Val
pattern VCNum n = VLit (LCNum n)

pattern VCPoly :: PolyMulti (Complex Frac) -> Val
pattern VCPoly n = VLit (LCPoly n)

pattern VTop :: Val
pattern VTop = VLit (LTop ())