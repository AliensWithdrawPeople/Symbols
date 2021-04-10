module Main where

import Base (parser, evaluator, exprShow)
import Equations (eqPars)

main :: IO ()     
main = do
      print ( eqPars "x^3 - 1 + 2")