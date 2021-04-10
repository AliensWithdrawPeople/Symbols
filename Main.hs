module Main where

import Base (parser, evaluator, exprShow)
import Equations (linearEqPars)

main :: IO ()     
main = do
      print ( linearEqPars "x^4 + x^5 - 1 + 2")