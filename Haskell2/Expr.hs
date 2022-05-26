module Expr where

import Parsing
import Data.Fixed

type Name = String

-- At first, 'Expr' contains only addition, conversion to strings, and integer
-- values. You will need to add other operations, and variables
data Expr = Add Expr Expr
          | Sub Expr Expr
          | Mult Expr Expr
          | Div Expr Expr
          | Abs Expr
          | Mod Expr Expr
          | Pow Expr Expr
          | Conc Expr Expr
          | ToString Expr
          | ToInt Expr
          | Val Value
          | Var Name
  deriving Show

--Added the ability for a value to be a double or boolean
data Value = IntVal Int | StrVal String | DoubVal Double | BoolVal Bool
  deriving Show

-- These are the REPL commands
data Command = Set Name Expr      -- assign an expression to a variable name
             | Print Expr         -- evaluate an expression and print the result
             | Input Name         -- assigns user's input to a variable name
             | Read String        -- reads a file and executes the commands it contains
             | Repeat Command Int -- repeats a command a certain no. of times
             | If BoolExpr Command Command -- Executes commands dependant on a boolean
             | Quit               -- terminates the program
  deriving Show

-- A Boolean expression conisting of two expresions, linked by a boolean operator
data BoolExpr = BExpr Expr String Expr
  deriving Show

eval :: [(Name, Value)] -> -- Variable name to value mapping
        Expr -> -- Expression to evaluate
        Either String Value -- Result (if no errors such as missing variables)
eval vars (Val x) = Right x -- for values, just give the value directly
eval vars (Var x)
  -- Checks if value is present, if so returns it, otherwise throws an error
  | elem x (map (\y -> fst y) vars) = Right (snd (head (filter (\y -> fst y == x) vars)))
  | otherwise = Left "error"
eval vars (Add x y) = case a of
  Right (DoubVal a) -> case b of
    Right (DoubVal b) -> Right (DoubVal (a+b))
    Right a -> Left "addition error"
    Left e -> Left "addition error"
  Right a-> Left "addition error"
  Left e -> Left "addition error"
  where
    a = eval vars x
    b = eval vars y
eval vars (Sub x y) = case a of
  Right (DoubVal a) -> case b of
    Right (DoubVal b) -> Right (DoubVal (a-b))
    Right a -> Left "subtraction error"
    Left e -> Left "subtraction error"
  Right a-> Left "subtraction error"
  Left e -> Left "subtraction error"
  where
    a = eval vars x
    b = eval vars y
eval vars (Mult x y) = case a of
  Right (DoubVal a) -> case b of
    Right (DoubVal b) -> Right (DoubVal (a*b))
    Right a -> Left "multiplication error"
    Left e -> Left "multiplication error"
  Right a-> Left "multiplication error"
  Left e -> Left "multiplication error"
  where
    a = eval vars x
    b = eval vars y
eval vars (Div x y) = case a of
  Right (DoubVal a) -> case b of
    Right (DoubVal b) -> Right (DoubVal (a/b))
    Right a -> Left "division error"
    Left e -> Left "division error"
  Right a-> Left "division error"
  Left e -> Left "division error"
  where
    a = eval vars x
    b = eval vars y
eval vars (ToString x) = case a of
  Right (DoubVal d) -> Right (StrVal (show d))
  Right (IntVal i) -> Right (StrVal (show i))
  Right (StrVal i) -> Right (StrVal i)
  Left e -> Left "string conversion error"
  where
    a = eval vars x
eval vars (ToInt x) = case a of
  Right (StrVal s) -> Right (DoubVal (read s))
  Right a -> Left "integer conversion error"
  Left e -> Left "integer conversion error"
  where
    a = eval vars x
eval vars (Conc x y) = case a of
  Right (StrVal a) -> case b of 
    Right (StrVal b) -> Right (StrVal (a++b))
    Right b -> Left "concatenation error"
    Left e -> Left "concatenation error"
  Right a -> Left "concatenation error"
  Left e -> Left "concatenation error"
  where
    a = eval vars x
    b = eval vars y
eval vars (Abs x) = case a of
  Right (DoubVal a) -> Right (DoubVal (abs a))
  Right a -> Left "modulus error"
  Left e -> Left "modulus error"
  where
    a = eval vars x
eval vars (Mod x y) = case a of
  Right (DoubVal a) -> case b of 
    Right (DoubVal b) -> Right (DoubVal (mod' a b))
    Right b -> Left "modulo operation error"
    Left e -> Left "modulo operation error"
  Right a -> Left "modulo operation error"
  Left e -> Left "modulo operation error"
  where
    a = eval vars x
    b = eval vars y
eval vars (Pow x y) = case a of
  Right (DoubVal a) -> case b of 
    Right (DoubVal b) -> Right (DoubVal (a**b))
    Right b -> Left "exponentiation error"
    Left e -> Left "exponentiation error"
  Right a -> Left "exponentiation error"
  Left e -> Left "exponentation error"
  where
    a = eval vars x
    b = eval vars y

digitToInt :: Char -> Int
digitToInt x = fromEnum x - fromEnum '0'

pBoolean :: Parser BoolExpr
pBoolean = do e1 <- pExpr
              space
              string "=="
              space
              e2 <- pExpr
              return (BExpr e1 "==" e2)
            ||| do  e1 <- pExpr
                    space
                    string ">"
                    space
                    e2 <- pExpr
                    return (BExpr e1 ">" e2)
            ||| do  e1 <- pExpr
                    space
                    string "<"
                    space
                    e2 <- pExpr
                    return (BExpr e1 "<" e2)
            ||| do  e1 <- pExpr
                    space
                    string ">="
                    space
                    e2 <- pExpr
                    return (BExpr e1 ">=" e2)
            ||| do  e1 <- pExpr
                    space
                    string "<="
                    space
                    e2 <- pExpr
                    return (BExpr e1 "!=" e2)
            ||| do  e1 <- pExpr
                    space
                    string "!="
                    space
                    e2 <- pExpr
                    return (BExpr e1 "!=" e2)
            

pCommand :: Parser Command
pCommand = do t <- many letter
              space
              char '='
              space
              string "input"
              return (Input t)
            ||| do string "if"
                   space
                   i <- pBoolean
                   space
                   string "then"
                   space
                   t <- pCommand
                   space
                   string "else"
                   space
                   e <- pCommand
                   space
                   return (If i t e)
            ||| do string "repeat"
                   space
                   n <- int
                   space
                   char '{'
                   space
                   c <- pCommand
                   char '}'
                   space
                   return (Repeat c n)
            ||| do t <- many letter
                   space
                   char '='
                   space
                   e <- pExpr
                   return (Set t e)
            ||| do string "print"
                   space
                   e <- pExpr
                   return (Print e)
            ||| do string "read"
                   space
                   f <- many filechar
                   return (Read f)
            ||| do string "quit"
                   return (Quit)

pExpr :: Parser Expr
pExpr = do t <- pTerm
           space
           do string "++"
              space
              e <- pExpr
              return (Conc t e)
            ||| do char '+'
                   space
                   e <- pExpr
                   return (Add t e) 
            ||| do char '-'
                   space
                   e <- pExpr
                   return (Sub t e)
            ||| do string "%"
                   space
                   e <- pExpr
                   return (Mod t e)
            ||| do string "^"
                   space
                   e <- pExpr
                   return (Pow t e)
                 ||| return t

pFactor :: Parser Expr
pFactor = do d <- int
             char '.'
             do i <- many digit
                return (Val (DoubVal (read((show d)++"."++i)::Double)))
           ||| do d <- int
                  return (Val (DoubVal (fromIntegral (d::Int) *1.0)))
           ||| do char '-'
                  d <- int
                  return (Val (IntVal (d*(-1))))
           ||| do string "toString"
                  space
                  char '('
                  space
                  e <- pExpr
                  char ')'
                  return (ToString e)
           ||| do string "toInt"
                  space
                  char '('
                  space
                  e <- pExpr
                  char ')'
                  return (ToInt e)
           ||| do string "Abs"
                  space
                  char '('
                  space
                  e <- pExpr
                  char ')'
                  return (Abs e)
           ||| do char '\"'
                  l <- many validString
                  char '\"'
                  space
                  return (Val (StrVal l))
           ||| do char '('
                  space
                  e <- pExpr
                  char ')'
                  return e
           ||| do v <- many letter
                  return (Var v)
           
pTerm :: Parser Expr
pTerm = do f <- pFactor
           space
           do char '*'
              space
              t <- pTerm
              return (Mult f t)
            ||| do char '/'
                   space
                   t <- pTerm
                   return (Div f t)
                 ||| return f