-- Module      : Symbols.Equation
-- Copyright   : (c) Daniel Ivanov 2021
-- License     : MIT
-- Maintainer  : daniilivanov1606@gmail.com
-- Stability   : experimental
-- Portability : requires GHC>7 extensions

module Equations
( linearEq
) where

import Base (evaluator)
import Data.List.Split.Internals (split, oneOf, dropDelims, keepDelimsL, dropInitBlank, onSublist, condense)
import Data.List (sortBy, elemIndex, takeWhile, dropWhile, groupBy, sort)
import Data.Char(isDigit)
import Data.Function(on)
import Data.Complex ()

data Expression = Poly [Member] | Shitty [String] 
instance Show Expression where
      show x = eqShow x
type Member =  (Float, Float)

-- Users functions
linearEq :: String -> Expression
linearEq = simplify . linearEqPars 

--Linear equation block: start
-- polynomialEquation :: String -> Float -> Float 
-- polynomialEquation = solver . simplify . linearEqPars

-- solver :: [(Float , Float)] -> Float -> Float 
-- solver lst x = foldl (\x) b (t a)

--Linear equation block: end

-- Simplify block: start
simplify :: [String] -> Expression
simplify lst = Poly (simply lst)  
    where simply = map (\x -> (read . fst $ x, read . snd $ x) ) . combine . map (evaluateFactor . separator . split (keepDelimsL . onSublist $ "x^"))

evaluateFactor :: (String, String) -> (String, String)
evaluateFactor (first, second)
    | first == "+" = ("1.0", second)
    | first == "-" = (first ++ "1.0", second)
    | head first == '+' || head first == '-' = ((show . maybeNumToNum . evaluator) ("0" ++ first), second)
    | otherwise  = (first, second)

combine :: [(String, String)] -> [(String, String)]
combine = filter (\x -> fst x /= "0.0") . map a . groupBy ((==) `on` snd)
    where a [singleton] = singleton
          a lst = foldr (\(accFirst, accSecond) (first, second) -> (show ((read accFirst :: Float) + (read first :: Float)), second)) ("0.0", (snd . head) lst) lst

separator :: [String] -> (String, String)
separator [first, second]
    | last first == '*' = (init first ++ (dropWhile isDigit . drop 2) second, drop 2 $ take 2 second ++ (takeWhile isDigit . drop 2) second)
    | otherwise  = (first ++ (dropWhile isDigit . drop 2) second, drop 2 $ take 2 second ++ (takeWhile isDigit . drop 2) second)
separator [first]
    | last first == 'x' = (init first, "1")
    | otherwise = (first, "0")
-- Simplify block: end

-- Linear Equation Parser block: start
linearEqPars :: String -> [String]
linearEqPars = concat . minusToSecond . map expressionParse . split (dropDelims . onSublist $"=")
    where minusToSecond lst = [head lst, map minus $last lst]
          minus str = '-' : tail str

expressionParse :: String -> [String]
expressionParse = sortBy comp . split' [] . firstSign . filter (/= ' ') where
    firstSign str
        | (head str /= '-') && (head str /= '+') = '+' : str
        | otherwise = str

split' :: [String]  -> String -> [String]
split' [] str
    | null str = []
    | otherwise = split' [[head str]] (tail str)
split' (h : t) str
    | null str = reverse $ map reverse (h : t)
    | (head str == '+' || head str == '-') && '(' `notElem` h && ')' `elem` h = split' (( head str : h) : t) (tail str)
    | (head str == '+' || head str == '-') && (('(' `elem` h && ')' `elem` h) || ('(' `notElem` h && ')' `notElem` h)) = split' ([head str] : h : t) (tail str)
    | otherwise = split' ((head str : h) : t) (tail str)

comp :: String -> String -> Ordering
comp a b
    | '^' `elem` a && '^' `elem` b && (degree a < degree b) = GT
    | '^' `elem` a && '^' `elem` b && (degree a > degree b) = LT
    | '^' `elem` a && '^' `notElem` b && 'x' `elem` b && (degree a > 1) = LT
    | '^' `elem` a && '^' `notElem` b && 'x' `elem` b && (degree a < 1) = GT
    | '^' `notElem` a && '^' `elem` b && 'x' `elem` b && (1 > degree b) = LT
    | '^' `notElem` a && '^' `elem` b && 'x' `elem` b && (1 < degree b) = GT
    | '^' `elem` a && '^' `notElem` b = GT
    | '^' `notElem` a && '^' `elem` b = LT
    | 'x' `elem` a && 'x' `notElem` b = LT
    | 'x' `notElem` a && 'x' `elem` b = GT
    | otherwise = LT
    where degree = nextEl . split (oneOf "^")
          nextEl lst = read (lst !! (maybeNumToNum (elemIndex "^" lst) + 1)) :: Float
-- Linear Equation Parser block: end

-- Supporting functions
maybeNumToNum :: (Num t) => Maybe t -> t
maybeNumToNum (Just n) = n
maybeNumToNum Nothing = 0

eqShow :: Expression -> String
eqShow (Poly pol) = concatMap memberToString pol ++ " = 0" where 
    memberToString memb 
        | fst memb >= 0  = "+" ++ show (fst memb) ++ "*x^" ++ show (snd memb)
        | fst memb < 0  = show (fst memb) ++ "*x^" ++ show (snd memb)
-- TODO
eqShow (Shitty sh) = "I can't show you this 'cause it's not ready yet."