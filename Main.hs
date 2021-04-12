module Main where

import Base (parser, evaluator, exprShow)
import Equations (linearEqPars, simplify)

main :: IO ()     
main = do
      print (simplify . linearEqPars $ "x^2 + 2x^1 + 1")