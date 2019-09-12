-- answer: 232792560

solve :: Int -> Int
solve limit = head . dropWhile (not . condition) $ [2520, 2522..]
  where
    condition x = all ((==) 0 . mod x) [1..limit]


main = do
  print . solve $ 20
