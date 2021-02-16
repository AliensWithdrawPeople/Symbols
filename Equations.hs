module Equations 
( eqPars
) where

import Base (parser, parser', Token)
import Data.List.Split.Internals (splitOn)

eqPars :: String -> [String] 
eqPars =  splitOn "+" . filter (/= ' ')