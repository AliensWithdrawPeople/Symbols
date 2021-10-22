module Main where

import Base (evaluator)
import Equations (linearEq, toFunc, at, polySolver)


main :: IO ()     
main = do
      print (toFunc "x^5 + 4x^4 + 8x^3 + 8x" `at` 2)
      print (polySolver "x^3 -3x^3 + 3x -5 = 0")