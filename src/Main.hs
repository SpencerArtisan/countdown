import Countdown

main = do
  putStrLn "Enter your source numbers, space separated"
  numberString <- getLine
  let numbers = map toInt $ words numberString
  putStrLn "Enter your target number"
  targetString <- getLine
  let target = read targetString
  mapM putStrLn $ countdownTargets numbers target

toInt :: String -> Int
toInt x = read x :: Int

