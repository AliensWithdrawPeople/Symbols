module Equations where

import Base (parser, Token)

simplifier :: [Token] -> [Token]
simplifier x = x