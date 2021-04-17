module Main where

import Base (evaluator)
import Equations (linearEqPars, simplify)

main :: IO ()     
main = do
      print ( simplify . linearEqPars $ "-(5+5)*x^2 + 2x + 1 + 2*3")