module Main where

import Base (evaluator)
import Equations (linearEq)


main :: IO ()     
main = do
      print (linearEq "3 + x^2 + 5x^3 + x = x^2")