module Main where

import Base (parser, evaluator, exprShow)
import Equations (linearEqPars, simplify)

main :: IO ()     
main = do
      print ( simplify . linearEqPars $ "5x^4/3 + x^5 - x^5 - 1 + 2")