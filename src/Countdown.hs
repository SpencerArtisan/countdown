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
    | null calcs = "No answer"
    | otherwise  = let Solution x s = head calcs in s
    where hitsTarget (Solution v r) = t == v
          calcs = filter hitsTarget $ countdown xs

countdownTargets :: [Int] -> Int -> [String]
countdownTargets xs t 
    | null calcs = []
    | otherwise  = map (\(Solution x s) -> s) calcs
    where hitsTarget (Solution v r) = t == v
          calcs = filter hitsTarget $ countdown xs


