module Countdown (countdown) where

import Combinator
import Operator

countdown :: [Int] -> [(Int,String)]
countdown [] = []
countdown xs = concat [operateWithMemo [plus,minus] (map toTuple cm) | ss <- subsets xs, cm <- combinations ss]
    where toTuple c = (c, show c)

