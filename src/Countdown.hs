module Countdown (countdown) where

import Combinator
import Operator

countdown :: [Int] -> [Int]
countdown [] = []
countdown [x] = [x]
countdown xs = concat [multiOperate [(+),(-)] cm | ss <- subsets xs, cm <- combinations ss]

