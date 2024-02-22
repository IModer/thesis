module Number where

{-  Number saját class hogy a polinómok
    class Number a => Polynomial a where
        ...
    lehessen
-}
class Number a where
    (+) :: a -> a -> a
--    (-) :: a -> a -> a
    (*) :: a -> a -> a
--    (/) :: a -> a -> a
-- ...


data Nat  = 
    Zero
    | Suc Nat

plusNat Zero y = y
plusNat (Suc x) y = Suc (plusNat x y)

timesNat Zero y = Zero
timesNat (Suc x) y = plusNat y $ timesNat x y

instance Number Nat where
    (+) = plusNat
    (*) = timesNat

{-
instance Number Integer where
    (+) = (+)
    (*) = (*)

-}