module Equations 
( linearEqPars
) where

import Base (parser, parser', Token)
import Data.List.Split.Internals (split, oneOf, keepDelimsL, dropInitBlank)
import Data.List (sortBy, elemIndex)

linearEqPars :: String -> [String]  
linearEqPars =  concatMap (split (dropInitBlank $ oneOf "+-")) . degreeSep

degreeSep :: String -> [String]
degreeSep =  sortBy comp . split (keepDelimsL $ oneOf "+-") . filter (/= ' ')

comp :: String -> String -> Ordering 
comp a b
    | '^' `elem` a && '^' `elem` b && (degree a < degree b) = GT
    | '^' `elem` a && '^' `elem` b && (degree a > degree b) = LT
    | '^' `elem` a && '^' `notElem` b = LT
    | '^' `notElem` a && '^' `elem` b = GT
    | otherwise = GT
    where degree = nextEl . split (oneOf "^*/")
          nextEl lst = read (lst !! (mbToInt (elemIndex "^" lst) + 1)) :: Float

mbToInt :: Maybe Int -> Int
mbToInt (Just n) = n
mbToInt Nothing = 0
