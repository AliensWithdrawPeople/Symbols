-- Module      : Symbols.Base
-- Copyright   : (c) Daniel Ivanov 2021
-- License     : MIT
-- Maintainer  : daniilivanov1606@gmail.com
-- Stability   : experimental
-- Portability : requires GHC>7 extensions

module Base 
( parser
, evaluator
, exprShow) where

import Data.List ( groupBy, all )
import Data.Char ( isDigit, isAlpha )

data Token = Atom Part | Func (Float -> Float) | Op (Float -> Float -> Float ) | Bracket Char 
data Part = Number Float | Var String   

instance Eq Token where
      Bracket '(' == Bracket '(' = True 
      Bracket ')' == Bracket ')' = True  
      _ == _ = False
      
instance Show Token where
      show x = tokenShow x

parser :: String -> [Token]
parser = map parseToken . filter (/= " ") . negNum [] . grouper
      where grouper =  groupBy (\x y -> (isDigit x && isDigit y) || (isAlpha x && isAlpha y) || (isDigit x && y == '.'))

parseToken :: [Char] -> Token
parseToken x 
      | x == "+" = Op (+)
      | x == "-" = Op (-)
      | x == "*" = Op (*)
      | x == "/" = Op (/)
      | x == "^" = Op (**)
      | x == "ln" = Func log
      | x == "log" = Func log
      | x == "sin" = Func sin
      | x == "(" =  Bracket '('
      | x == ")" =  Bracket ')'
      | all isAlpha x = (Atom . Var) x
      | otherwise   = (Atom . Number . read) x

-- shuntingYard :: input queue -> output queue -> operators stack -> output queue in rpn
shuntingYard :: [Token] -> [Token] -> [Token] -> [Token]
shuntingYard (Atom (Number n) : xs) out ops = shuntingYard xs (Atom (Number n) : out) ops
shuntingYard (Func f : Atom (Number n) : xs) out ops = shuntingYard xs ((Atom . Number . f) n : out) ops
shuntingYard (Func f : xs) out ops = shuntingYard xs out (Func f : ops)
shuntingYard (Op op1 : xs) out (x : ops) = if (opsPriority . Op) op1 <= opsPriority x then shuntingYard (Op op1 : xs) (x : out) ops
                                                else shuntingYard xs out (Op op1 : x : ops)
shuntingYard (Op op1 : xs) out ops = shuntingYard xs out (Op op1 : ops)
shuntingYard (Bracket '(' : xs) out ops = shuntingYard xs out (Bracket '(' : ops)
shuntingYard (Bracket ')' : xs) out (x : ops) = if x == Bracket '(' then shuntingYard xs out ops
                                                else shuntingYard (Bracket ')' : xs) (x : out) ops
shuntingYard [] out (x : ops) = shuntingYard [] (x : out) ops
shuntingYard [] out [] = reverse out

solveRPN :: [Token] -> Token 
solveRPN = head . foldl foldingFunction []  
    where   foldingFunction (Atom (Number x1) : xs) (Func func) = (Atom . Number . func) x1 : xs
            foldingFunction (Atom (Number x1) : Atom (Number x2) : xs) (Op op) = (Atom . Number) (op x2 x1) : xs
            foldingFunction xs ys = ys : xs

evaluator :: String -> Maybe Float 
evaluator xs
      | null xs = Nothing
      | (any varCheck . parser) xs = Nothing  
      | otherwise  = Just $ (toNum . solveRPN) $ shuntingYard (parser xs) [] []


-- helper functions

tokenShow :: Token -> String 
tokenShow (Op op)
      | op 2 3 == 2 ** 3 = " ^ "
      | op 2 3 == 2 * 3 = " * "
      | op 2 3 == 2 / 3 = " / "
      | op 2 3 == 2 + 3 = " + "
      | op 2 3 == 2 - 3 = " - "
tokenShow (Func f)
      | f 2 == log 2 = " ln"
      | f 2 == sin 2 = " sin"
tokenShow (Bracket x) = " " ++ (x : " ")
tokenShow (Atom (Number x)) = show x
tokenShow (Atom (Var x)) = x

negNum :: [String] -> [String] -> [String]
negNum out ("-" : y : xs) = negNum (("-" ++ y) : "+" : "0" : out) xs
negNum out (x : xs) = negNum (x : out) xs
negNum out [] = reverse out

opsPriority :: Token -> Int  
opsPriority (Op op)
      | op 2 3 == 2 ** 3 = 3
      | op 2 3 == 2 * 3 = 2
      | op 2 3 == 2 / 3 = 2
      | op 2 3 == 2 + 3 = 1
      | op 2 3 == 2 - 3 = 1
opsPriority (Func f) = 4
opsPriority x = 0

toNum :: Token -> Float 
toNum (Atom (Number x)) = x

varCheck :: Token -> Bool 
varCheck (Atom (Var x)) = True
varCheck x = False

exprShow :: [Token] -> String 
exprShow = concatMap show