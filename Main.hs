module Main where

import Data.List ( groupBy, all )
import Data.Char ( isDigit, isAlpha )
import Base (parser, evaluator, exprShow)

main :: IO ()     
main = do
      print ( (evaluator . exprShow . parser) "1-(1+2)")