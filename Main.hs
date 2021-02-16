module Main where

import Base (parser, evaluator, exprShow)
import Equations (eqPars)

main :: IO ()     
main = do
      print ( eqPars "x^2 - 1 + 2")