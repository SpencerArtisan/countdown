module Countdown (countdown) where

import Combinator
import Operator

countdown :: [Int] -> [(Int,String)]
countdown [] = []
countdown xs = concat [operateWithMemo [plus,minus] (map (\c -> (c,show c)) cm) | ss <- subsets xs, cm <- combinations ss]

