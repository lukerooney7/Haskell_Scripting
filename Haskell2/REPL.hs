module REPL where

import Expr
import Parsing
import Data.Fixed
import System.Exit

data LState = LState { vars :: [(Name, Value)] }

-- Binary Tree implementation
data Tree a = Leaf | Node (Tree a) a ( Tree a)

instance Eq a => Eq (Tree a) where
     (==) Leaf Leaf = True
     (==) (Node l v r) (Node l' v' r')
                    = l == l' && v == v' && r == r'
     (==) _ _ = False

initLState :: LState
initLState = LState []

-- A backlog of commands that are added to when reading a file or looping
initCommands :: [Command]
initCommands = []

-- If it already exists, remove the old value
updateVars :: Name -> Value -> [(Name, Value)] -> [(Name, Value)]
updateVars name val vars = if elem name (map (\x -> fst x) vars)
     then 
          map (\x -> if fst x == name then (name, val) else x) vars
     else
          vars ++ [(name, val)]

-- Return a new set of variables with the given name removed ????????????????/
dropVar :: Name -> [(Name, Value)] -> [(Name, Value)]
dropVar = undefined

-- Contains multiple instances depending on the command to be executed
process :: LState -> Command -> [Command] -> IO ()
process st (Set var e) commands
     = do let st' = case num of 
               --Stores the variable if the expression is evaluated succesfully
               Right int -> st { vars = updateVars var int (vars st) }
               Left e -> st
               where
                    --Evaluates the expression that will be assigned to var
                    num = eval (vars st) e 
          repl st' commands -- Passes the new state back into repl
process st (Print e) commands
     = do let res = case num of
                Right (StrVal num) -> num
                Right (IntVal num) -> show num --Changes int to string
                Right (DoubVal num) -> case mod' num 1 of
                  -- If the double is an integer, converts it to an int type 
                  0 -> show (round num :: Int)
                  otherwise -> show num
                Left e -> e
                where
                     --Evaluates the expression that will be printed
                     num = eval (vars st) e
          --Prints result in correct format, then calls repl to continue loop
          putStrLn res
          repl st commands
process st (Input var) commands
     = do inp <-getLine -- Gets user's next input
          --Calls Set on the user's input
          process st (Set var (Val (StrVal inp))) commands 
          repl st commands
process st (Read fname) commands
     = do f <- (readFile fname) -- Opens file
          let l = lines f --Splits file into a list of strings, split by new line
          let list = parseList l
          case list of
               [] -> do  putStrLn "Error: Empty File"
                         repl st commands
               _ -> repl st (commands ++ list) --Adds the new commands to the back log
process st (If (BExpr e1 operator e2) t e) commands
    = do let res = case b1 of 
              -- Ensures that numbers are only compared with numbers
              Right (DoubVal v1) -> case b2 of
                Right (DoubVal v2) -> case operator of 
                  -- Each parsed case matches the built in comparison
                  "==" -> v1 == v2
                  ">" -> v1 > v2 
                  "<" -> v1 < v2 
                  ">=" -> v1 >= v2 
                  "<=" -> v1 <= v2 
                  "!=" -> not (v1 == v2) 
         -- If the boolean comparison is true, execute the then command,
         -- otherwise execute the else command
         if res then process st t commands else process st e commands               
         repl st commands
          where
               b1 = eval (vars st) e1
               b2 = eval (vars st) e2
process st (Repeat e n) commands
     = do if n > 0 --Checks that loop will be run at least once
           -- recalls process, after adding the repeated command to the backlog
           -- This iwill recursively repeat until n reaches 0
           then process st (Repeat e (n-1)) (commands++[e])
          else if n == 0
            then repl st commands -- Base case of 'Repeat' recursion
          --If no. of repititions is negative, print an error message
          else do putStrLn "Error: Number of repititions must be > 0"
                  repl st commands
process st (Quit) commands
      -- Prints exit message before teminating the program
      = die "Exiting..."

-- Implemented function that takes a list of strings, and returns the parsed commands
parseList :: [String] -> [Command]
parseList [] = []
parseList [c] = case parse pCommand c of
                                        [(cmd, "")] -> [cmd]
                                        _ -> [Print (Val (StrVal "Parse Error"))]
parseList (c:cs) = case parse pCommand c of
                                        [(cmd, "")] -> [cmd] ++ (parseList cs)
                                        _ -> do [Print (Val (StrVal "Parse Error"))] ++ (parseList cs)

-- Read, Eval, Print Loop
-- This reads and parses the input using the pCommand parser, and calls
-- 'process' to process the command.
-- 'process' will call 'repl' when done, so the system loops.

repl :: LState -> [Command] -> IO ()
repl st commands = do 
                    if (length commands) == 0 -- Checks if there are commands in backlog
                         then do inp <- getLine
                                 putStr ("> ")
                                 case parse pCommand inp of
                                        [(cmd, "")] -> -- Must parse entire input
                                                  process st cmd commands
                                        _ -> do putStrLn "Parse error"
                                                repl st commands
                         else process st (head commands) (tail commands)
                         