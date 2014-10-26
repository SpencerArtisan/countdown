module Countdown (countdown, countdownTarget) where

import Combinator
import Operator

countdown :: [Int] -> [Solution]
countdown [] = []
countdown xs = concat [operateWithMemo ops (map toSolution cm) | ss <- subsets xs, cm <- combinations ss]
    where toSolution c = Solution c $ show c
          ops = [plus, minus, times]

countdownTarget :: [Int] -> Int -> String
countdownTarget xs t 
    | null calcs = "No answer"
    | otherwise  = let Solution x s = head calcs in s
    where hitsTarget (Solution v r) = t == v
          calcs = filter hitsTarget $ countdown xs


