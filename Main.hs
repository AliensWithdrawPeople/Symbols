module Main where

import Base (evaluator)
import Equations (linearEq, toFunc, at)


main :: IO ()     
main = do
      print ((toFunc "x^5 + 4x^4 + 8x^3 + 8x") `at` 2)