{-# LANGUAGE TemplateHaskell #-}
module Tests where

import Test.QuickCheck
import Test.QuickCheck.All
import Data.Fixed
-- import Main
import Expr


instance Arbitrary Value where
    arbitrary = elements[(DoubVal 1),(DoubVal 3),(DoubVal 10),(DoubVal 78),(DoubVal 104.43),(DoubVal 0.1526),(DoubVal 9989),(DoubVal (-292)),(DoubVal (-22)),(DoubVal (-0.234))]

prop_add :: Value -> Value -> Bool
prop_add (DoubVal x) (DoubVal y) = res ==  x + y
    where
        Right (DoubVal res) = eval [] (Add (Val (DoubVal x)) (Val (DoubVal y)))
prop_subtract :: Value -> Value -> Bool
prop_subtract (DoubVal x) (DoubVal y) = res ==  x - y
    where
        Right (DoubVal res) = eval [] (Sub (Val (DoubVal x)) (Val (DoubVal y)))
prop_multiply :: Value -> Value -> Bool
prop_multiply (DoubVal x) (DoubVal y) = res ==  x * y
    where
        Right (DoubVal res) = eval [] (Mult (Val (DoubVal x)) (Val (DoubVal y)))
prop_divide :: Value -> Value -> Bool
prop_divide (DoubVal x) (DoubVal y) = res ==  x / y
    where
        Right (DoubVal res) = eval [] (Div (Val (DoubVal x)) (Val (DoubVal y)))
prop_power :: Value -> Value -> Bool
prop_power (DoubVal x) (DoubVal y) 
    | isNaN res = True
    | otherwise = res ==  x ** y
    where
        Right (DoubVal res) = eval [] (Pow (Val (DoubVal x)) (Val (DoubVal y)))
prop_modulo :: Value -> Value -> Bool
prop_modulo (DoubVal x) (DoubVal y) = res ==  mod' x y
    where
        Right (DoubVal res) = eval [] (Mod (Val (DoubVal x)) (Val (DoubVal y)))
prop_absolute :: Value -> Bool
prop_absolute (DoubVal x) = res == abs x
    where
        Right (DoubVal res) = eval [] (Abs (Val (DoubVal x)))
prop_concat :: Value -> Value -> Bool
prop_concat (DoubVal x) (DoubVal y) = res ==  (show x) ++ (show y)
    where
        Right (StrVal res) = eval [] (Conc (Val (StrVal (show x))) (Val (StrVal (show y))))
prop_toString :: Value -> Bool
prop_toString (DoubVal x) = res == show x
    where
        Right (StrVal res) = eval [] (ToString (Val (DoubVal x)))
prop_toInt :: Value -> Bool
prop_toInt (DoubVal x) = res == x
    where
        Right (DoubVal res) = eval [] (ToInt (Val (StrVal (show x))))

return []
check = $quickCheckAll