module Countdown (countdown, countdownTarget, countdownTargets) where

import Combinator
import Operator
import Data.List

countdown :: [Int] -> [Solution]
countdown [] = []
countdown xs = concat [operateWithMemo ops (map toSolution cm) | ss <- subsets xs, cm <- combinations ss]
    where toSolution c = Solution c $ show c
          ops = [plus, minus, times, divide]

countdownTarget :: [Int] -> Int -> String
countdownTarget xs t
    | null results = "No answer"
    | otherwise    = head results
    where results = countdownTargets xs t

countdownTargets :: [Int] -> Int -> [String]
countdownTargets xs t 
    | null calcs = []
    | otherwise  = nub $ map extractMemo calcs
    where hitsTarget (Solution v r) = t == v
          calcs = filter hitsTarget $ countdown xs
          extractMemo (Solution v r) = r


