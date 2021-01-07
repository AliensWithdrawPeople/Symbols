module Main where

import Data.List ( groupBy )
import Data.Char ( isDigit, isAlpha, isSpace )
data Token = Atom Float | Func (Float -> Float) | Op (Float -> Float -> Float ) | Bracket Char

parser :: String -> [Token]
parser = map parseToken . filter (/= " ") . groupBy (\x y -> (isDigit x && isDigit y) || (isAlpha x && isAlpha y) || (isDigit x && y == '.')) 

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
      | otherwise  = Atom (read x)

-- shuntingYard :: input queue -> output queue -> operators stack -> output queue in rpn
shuntingYard :: [Token] -> [Token] -> [Token] -> [Token]
shuntingYard (Atom n : xs) out ops = shuntingYard xs (Atom n : out) ops
shuntingYard (Func f : Atom n : xs) out ops = shuntingYard xs ((Atom . f) n : out) ops
shuntingYard (Func f : xs) out ops = shuntingYard xs out (Func f : ops)
shuntingYard (Op op1 : xs) out (x : ops) = if (opsPriority . Op) op1 <= opsPriority x then shuntingYard (Op op1 : xs) (x : out) ops
                                                else shuntingYard xs out (Op op1 : x : ops)
shuntingYard (Op op1 : xs) out ops = shuntingYard xs out (Op op1 : ops)
shuntingYard (Bracket '(' : xs) out ops = shuntingYard xs out (Bracket '(' : ops)
shuntingYard (Bracket ')' : xs) out (x : ops) = if bracketEQ '(' x then shuntingYard xs out ops
                                                else shuntingYard (Bracket ')' : xs) (x : out) ops
shuntingYard [] out (x : ops) = shuntingYard [] (x : out) ops
shuntingYard [] out [] = reverse out

solveRPN :: [Token] -> Token 
solveRPN = head . foldl foldingFunction []  
    where   foldingFunction (Atom x1 : xs) (Func func) = Atom (func x1) : xs
            foldingFunction (Atom x1 : Atom x2 : xs) (Op op) = Atom (op x2 x1) : xs
            foldingFunction xs ys = ys : xs

bracketEQ :: Char -> Token -> Bool 
bracketEQ a (Bracket x) = a == x
bracketEQ a t = False

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
toNum (Atom x) = x

evaluater :: String -> Maybe Float 
evaluater [] = Nothing 
evaluater xs = Just ((toNum . solveRPN) (shuntingYard (parser xs) [] []))

main :: IO ()     
main = do
      print ( evaluater "1")