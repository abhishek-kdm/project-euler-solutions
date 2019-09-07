solve :: Int -> Int
solve n = sum . filter (\x -> (multiple 3 x) || (multiple 5 x)) $ [1..(n-1)]
  where 
    multiple x = (== 0) . (\x' -> mod x' x)

main = do
  print . solve $ 1000
