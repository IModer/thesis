{-# LANGUAGE OverloadedStrings #-}
module Main where

import Test.HUnit
import qualified System.Exit as Exit

import Core.TypeChecker
import Core.Interpreter
import Core.Types
import Core.AST
import Core.Classes
import Lib
import Parser

import Data.Text hiding (zipWith)
import Data.Functor.Classes
import Data.Semiring
import Data.Euclidean
import Data.Either (isLeft, isRight)

import Control.Monad.Except (throwError, runExceptT)
import Control.Monad.State
import Control.Monad.Identity

-- Shorthands

tTInt i = TLit $ LCNum (i %% 1 :+ 0 %% 1)
tTFrac i j= TLit $ LCNum (i %% j :+ 0 %% 1)
tTBool  = TLit . LBool
tTtt    = TLit $ LTop ()
runErrorT = runIdentity . flip runStateT emptyEnv . runExceptT

-- Custom instance for testing
instance Eq1 (StateT GEnv Identity) where
    liftEq f fa fb = f a' b'
        where
            (a', e1) = runIdentity $ runStateT fa emptyEnv
            (b', e2) = runIdentity $ runStateT fb emptyEnv
            

instance Show1 (StateT GEnv Identity) where
    liftShowsPrec f as i fa = f i a'
        where
            (a', e1) = runIdentity $ runStateT fa emptyEnv

instance Eq (TTm) where
    TVar x == TVar x' = x == x'
    TLam x t u == TLam x' t' u' = x == x' && t == t' && u == u'
    TApp e1 e2 == TApp e1' e2' = e1 == e1' && e2 == e2' 
    TLet x y u == TLet x' y' u' = x == x' && y == y' && u == u'
    TIfThenElse e1 e2 e3 == TIfThenElse e1' e2' e3' = e1 == e1' && e2 == e2' && e3 == e3'
    TLit l == TLit l' = l == l'
    TListCons t ts == TListCons t' ts' = t == t' && ts == ts'
    TBinPred op _ e1 e2 == TBinPred op' _ e1' e2' = e1 == e1' && e2 == e2' && op == op'
    TBinOpBool op _ e1 e2 == TBinOpBool op' _ e1' e2' = e1 == e1' && e2 == e2' && op == op'
    TBinFieldOp op _ e1 e2 == TBinFieldOp op' _ e1' e2' = e1 == e1' && e2 == e2' && op == op'
    TBinEucOp op _ e1 e2 == TBinEucOp op' _ e1' e2' = e1 == e1' && e2 == e2' && op == op'
    TBinRingOp op _ e1 e2 == TBinRingOp op' _ e1' e2' = e1 == e1' && e2 == e2' && op == op'
    TPrefix op e1 == TPrefix op' e1' = e1 == e1' && op == op'
    TFix t == TFix t' = t == t'
    _ == _ = False


instance Eq Command where
    PrintHelp == PrintHelp = True
    RunTimed t == RunTimed t' = t == t'
    LoadFile fs == LoadFile fs' = fs == fs'
    GetType t == GetType t' = t == t'
    GetInfo t == GetInfo t' = t == t'
    ReloadFiles == ReloadFiles = True
    _ == _ = False

instance Eq TopDef where
    LetDef n t == LetDef n' t' = t == t' && n == n'
    VarDef x == VarDef x' = x == x'
    OpenDef x == OpenDef x' = x == x'
    CloseDef == CloseDef = True
    _ == _ = False

tm = Right . Right . OLeft
com = Right . Right . OMiddle
def = Right . Right . ORight

tm' a  = Right $ [Left a]
def' a = Right $ [Right a]

-- Typechecker testing 

-- Test for literals
-- :t 3 -- == Num
typeCheckT1 :: Test
typeCheckT1 = TestCase 
    (assertEqual "should be TCNum" 
        (return TCNum) 
        (typeCheck [] (tTInt 3)))

-- :t X * X + 4 -- == Poly
typeCheckT2 :: Test
typeCheckT2 = TestCase 
    (assertEqual "should be TCPoly" 
        (return TCPoly) 
        (typeCheck [] (TLit $ LCPoly (x `times` x `plus` four))))
    where
        Just x = stringToComplexPoly "X"
        four = complexToComplexPoly (4 %% 1 :+ 0 %% 1)

-- :t True -- == Bool
typeCheckT3 :: Test
typeCheckT3 = TestCase 
    (assertEqual "should be TBool" 
        (return TBool) 
        (typeCheck [] (tTBool True)))

-- :t [] -- == List
typeCheckT4 :: Test
typeCheckT4 = TestCase 
    (assertEqual "should be List" 
        (return TList) 
        (typeCheck [] (TLit $ LList []))) 

-- :t tt -- == Top
typeCheckT5 :: Test
typeCheckT5 = TestCase 
    (assertEqual "should be Top" 
        (return TTop) 
        (typeCheck [] (tTtt))) 

-- komplexebb típusok

-- :t (\x . Bool . x) -- == Bool -> Bool
typeCheckT6 :: Test
typeCheckT6 = TestCase 
    (assertEqual "should be Bool -> Bool" 
        (return $ TBool ~> TBool) 
        (typeCheck [] (TLam "x" TBool (TVar "x")))) 

-- :t x -- == Num
typeCheckT7 :: Test
typeCheckT7 = TestCase
    (assertEqual "should be TCNum" 
        (return TCNum) 
        (typeCheck [("x", TCNum)] (TVar "x")))

-- :t (\x : Bool . x | True) True -- == Bool 
typeCheckT8 :: Test
typeCheckT8 = TestCase 
    (assertEqual "should be TBool" 
        (return TBool) 
        (typeCheck [] (TApp 
                        (TLam "x" TBool 
                            (TBinOpBool Or (||) 
                                (TVar "x") 
                                (tTBool True)))
                        (tTBool True)
                        )
        )
    ) 

-- :t (if True then True else tt) -- == Bool
typeCheckT9 :: Test
typeCheckT9 = TestCase 
    (assertEqual "should be TBool" 
        (return TBool) 
        (typeCheck [] (TIfThenElse (tTBool True) (tTBool True) (tTBool False)))) 

-- Tesztek hibákra

-- :t (if True then True else tt) -- == Type error
typeCheckT10 :: Test
typeCheckT10 = TestCase 
    (assertBool "should be a type error"
        (isLeft $ 
        fst $ 
        runErrorT (typeCheck [] (TIfThenElse (tTBool True) (tTBool True) (tTtt)))))

-- :t x -- == Type error
typeCheckT11 :: Test
typeCheckT11 = TestCase
    (assertBool "should be a type error"
        (isLeft $
        fst $
        runErrorT (typeCheck [] (TVar "x"))))

-- :t x -- == Type error
typeCheckT12 :: Test
typeCheckT12 = TestCase
    (assertBool "should be a type error"
        (isLeft $
        fst $
        runErrorT (typeCheck [("x", TBool ~> TTop), ("y", TTop)] (TApp (TVar "x") (TVar "y")))))

typeCheckTests :: Test
typeCheckTests = TestList $
    zipWith (\i t -> TestLabel ("Type Checker test " ++ show i) t) 
        [1..] 
    [ typeCheckT1, typeCheckT2, typeCheckT3
    , typeCheckT4, typeCheckT5, typeCheckT6
    , typeCheckT7, typeCheckT8, typeCheckT9
    , typeCheckT10, typeCheckT11, typeCheckT12
    ]

-- Interpreter testing

-- Test for literals
-- 2 -- == 2
interpreterT1 :: Test
interpreterT1 = TestCase $
    assertEqual "should be 2" 
        (return $ tTInt 2)
        (runTypedTerm $ tTInt 2)

-- 2 + 5 -- == 7
interpreterT2 :: Test
interpreterT2 = TestCase $
    assertEqual "should be 7" 
        (return $ tTInt 7)
        (runTypedTerm $ TBinRingOp Plus plus (tTInt 2) (tTInt 5))

-- 3 / 8 -- == 3 / 8
interpreterT3 :: Test
interpreterT3 = TestCase $
    assertEqual "should be 3/8" 
        (return $ tTFrac 3 8)
        (runTypedTerm $ TBinFieldOp Div ((unsafe .) . divide) (tTInt 3) (tTInt 8) )

-- Test for literals
-- (\x : Bool . x) True -- == True
interpreterT4 :: Test
interpreterT4 = TestCase $
    assertEqual "should be True" 
        (return $ tTBool True)
        (runTypedTerm $ TApp (TLam "x" TBool (TVar "x")) (tTBool True))

-- if True then 3 else 2 -- == 3
interpreterT5 :: Test
interpreterT5 = TestCase $
    assertEqual "should be 3" 
        (return $ tTInt 3)
        (runTypedTerm $ TIfThenElse (tTBool True) (tTInt 3) (tTInt 2))

-- fix (\x : Bool . False) -- == True
interpreterT6 :: Test
interpreterT6 = TestCase $
    assertEqual "should be False" 
        (return $ tTBool False)
        (runTypedTerm $ TFix (TLam "x" TBool (tTBool False)) )

-- Stuck terms

-- Test for literals
-- (\x : Bool . x) -- == (\x : Bool . x)
interpreterT7 :: Test
interpreterT7 = TestCase $
    assertEqual "should be (\\x : Bool . x)" 
        (return $ (TLam "x" TBool (TVar "x")))
        (runTypedTerm $ (TLam "x" TBool (TVar "x")))

-- x -- == error
interpreterT8 :: Test
interpreterT8 = TestCase $
    assertBool "should be \"cannot find x\"" 
        (isLeft $
        fst $
        runErrorT $
        runTypedTerm $ TVar "x")

-- (\f : Bool -> Bool . fix f) -- == (\f : Bool -> Bool . fix f)
interpreterT9 :: Test
interpreterT9 = TestCase $
    assertEqual "should be (\\f : Bool -> Bool . fix f)"
        (return $ (TLam "f" (TBool ~> TBool) (TFix (TVar "f"))))
        (runTypedTerm $ (TLam "f" (TBool ~> TBool) (TFix (TVar "f"))) )

{- TODO? more tests
-- Test for literals
-- (\x : Bool . x) -- == (\x : Bool . x)
interpreterT10 :: Test
interpreterT10 = TestCase $
    assertEqual "should be (\\x : Bool . x)" 
        (return $ (TLam "x" TBool (TVar "x")))
        (runTypedTerm $ (TLam "x" TBool (TVar "x")))

-- x -- == error
interpreterT11 :: Test
interpreterT11 = TestCase $
    assertBool "should be \"cannot find x\"" 
        (isLeft $
        fst $
        runErrorT $
        runTypedTerm $ TVar "x")

-- (\f : Bool -> Bool . fix f) -- == (\f : Bool -> Bool . fix f)
interpreterT12 :: Test
interpreterT12 = TestCase $
    assertEqual "should be (\\f : Bool -> Bool . fix f)"
        (return $ (TLam "f" (TBool ~> TBool) (TFix (TVar "f"))))
        (runTypedTerm $ (TLam "f" (TBool ~> TBool) (TFix (TVar "f"))) )
-}


interpreterTests :: Test
interpreterTests = TestList $
    zipWith (\i t -> TestLabel ("Iterpreter test " ++ show i) t) 
        [1..]
    [ interpreterT1, interpreterT2, interpreterT3
    , interpreterT4, interpreterT5, interpreterT6
    , interpreterT7, interpreterT8, interpreterT9
    ]

-- Parser testing

-- Repl parsing successful

-- if True then 3 else 2
parserT1 :: Test
parserT1 = TestCase $
    assertEqual "should be TIfThenElse True 3 2" 
        (tm $ TIfThenElse (tTBool True) (tTInt 3) (tTInt 2))
        (parseStringRepl "if True then 3 else 2")

-- (\x y : Top . x)
parserT2 :: Test
parserT2 = TestCase $
    assertEqual "should be TLam x TTop (TLam y TTop (TVar x))" 
        (tm $ TLam "x" TTop (TLam "y" TTop (TVar "x")))
        (parseStringRepl "(\\x y : Top . x)")

-- (a b c)
parserT3 :: Test
parserT3 = TestCase $
    assertEqual "should be TApp (TApp (TVar a) (TVar b)) (TVar c)" 
        (tm $ TApp (TApp (TVar "a") (TVar "b")) (TVar "c"))
        (parseStringRepl "a b c") 

-- def a := 2
parserT3' :: Test
parserT3' = TestCase $
    assertEqual "should be LetDef a 2" 
        (def $ LetDef "a" (tTInt 2))
        (parseStringRepl "def a :=   2")

-- Repl parsing fail

-- (\x y . x)
parserT4 :: Test
parserT4 = TestCase $
    assertBool "should be a parsing error" 
        (isLeft $ parseStringRepl "(\\x y . x)")


-- 3:
parserT5 :: Test
parserT5 = TestCase $
    assertBool "should be a parsing error" 
        (isLeft $ parseStringRepl "3:")

-- def if := 2
parserT6 :: Test
parserT6 = TestCase $
    assertBool "should be a parsing error" 
        (isLeft $ parseStringRepl "def if := 2")

-- File parsing successful

parserT7 :: Test
parserT7 = TestCase $
    assertEqual "should be TIfThenElse True 3 2" 
        (tm' $ TIfThenElse (tTBool True) (tTInt 3) (tTInt 2))
        (parseStringFile "" "if True then 3 else 2")

-- (\x y : Top . x)
parserT8 :: Test
parserT8 = TestCase $
    assertEqual "should be TLam x TTop (TLam y TTop (TVar x))" 
        (tm' $ TLam "x" TTop (TLam "y" TTop (TVar "x")))
        (parseStringFile "" "(\\x y : Top . x)")

-- (a b c)
parserT9 :: Test
parserT9 = TestCase $
    assertEqual "should be TApp (TApp (TVar a) (TVar b)) (TVar c)" 
        (def' $ LetDef "a" (tTInt 2))
        (parseStringFile "" "def a := 2")

-- def a := 2
parserT9' :: Test
parserT9' = TestCase $
    assertEqual "should be LetDef a 2" 
        (tm' $ TApp (TApp (TVar "a") (TVar "b")) (TVar "c"))
        (parseStringFile "" "a b c")

parserTests :: Test
parserTests = TestList $
    zipWith (\i t -> TestLabel ("Parser test " ++ show i) t) 
        [1..] 
    [ parserT1, parserT2, parserT3, parserT3'
    , parserT4, parserT5, parserT6 
    , parserT7, parserT8, parserT9, parserT9'
    ]

--tests :: Test
tests = TestList 
    [ typeCheckTests
    , interpreterTests
    , parserTests ]

main :: IO ()
main = do
    result <- runTestTT tests

    if  failures result > 0 
        then Exit.exitFailure 
        else Exit.exitSuccess