module Main where

import Base ( evaluator )
import Equations ( linearEq, toFunc, at, polySolver, funcPars, binomialExpansion )
import Data.Complex ( magnitude )

main :: IO ()     
main = do
      -- print (polySolver "x^3 -3x^2 +3x - 5 = 0" 1e-3)
      -- print (map (\x -> magnitude $toFunc "x^3 -3x^2 +3x - 5" `at` x) (polySolver "x^3 -3x^2 +3x - 5 = 0" 1e-3))
      print (funcPars "2(x) ^ 2 + 3")
      print (binomialExpansion "x-3" 2)