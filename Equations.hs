module Equations 
( eqPars
) where

import Base (parser, parser', Token)
import Data.List.Split.Internals (split, oneOf)

eqPars :: String -> [String] 
eqPars =  split (oneOf "^*/+-") . filter (/= ' ')