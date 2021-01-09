module Main where

import Data.List ( groupBy, all )
import Data.Char ( isDigit, isAlpha )
import Base (evaluator)

main :: IO ()     
main = do
      print ( evaluator "ln(-1+2) + 2 + 2*2^2 + 53")