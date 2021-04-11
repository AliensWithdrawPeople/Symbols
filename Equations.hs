module Equations 
( linearEqPars
, simplify
) where

import Base (evaluator, Token)
import Data.List.Split.Internals (split, oneOf, keepDelimsL, dropInitBlank, onSublist)
import Data.List (sortBy, elemIndex, takeWhile, dropWhile)
import Data.Char(isDigit)

-- Simplify block: start
simplify :: [String] -> [(String, String)]
simplify = map $ ev . separator . split (keepDelimsL . onSublist $ "x^")

ev :: (String, String) -> (String, String)
ev (first, second) 
    | first == "+" || first == "-" = (first ++ "1.0", second)
    | head first == '+' || head first == '-' = ((show . maybeNumToNum . evaluator) ("0" ++ first), second)
    | otherwise  = (first, second)


separator :: [String] -> (String, String)         
separator [first, second] = (first ++ (dropWhile isDigit . drop 2) second, take 2 second ++ (takeWhile isDigit . drop 2) second)
separator [first] = (first, "x^0") -- (show . evaluator)
-- Simplify block: end

-- Linear Equation Parser block: start
linearEqPars :: String -> [String]  
linearEqPars =  concatMap (split (dropInitBlank . keepDelimsL $ oneOf "+-")) . degreeSep

degreeSep :: String -> [String]
degreeSep =  sortBy comp . split (keepDelimsL $ oneOf "+-") . sign . filter (/= ' ')
    where sign str 
            | head str /= '-' = "+" ++ str
            | otherwise = str

comp :: String -> String -> Ordering 
comp a b
    | '^' `elem` a && '^' `elem` b && (degree a < degree b) = GT
    | '^' `elem` a && '^' `elem` b && (degree a > degree b) = LT
    | '^' `elem` a && '^' `notElem` b = LT
    | '^' `notElem` a && '^' `elem` b = GT
    | otherwise = GT
    where degree = nextEl . split (oneOf "^*/")
          nextEl lst = read (lst !! (maybeNumToNum (elemIndex "^" lst) + 1)) :: Float
-- Linear Equation Parser block: end

-- Supporting functions
maybeNumToNum :: (Num t) => Maybe t -> t
maybeNumToNum (Just n) = n
maybeNumToNum Nothing = 0