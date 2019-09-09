-- answer: 4613732

fib :: [Int]
fib = 1 : 2 : zipWith (+) fib (tail fib)

solve :: Int -> Int
solve n = sum . filter (\x -> x `mod` 2 == 0) . takeWhile (<= n) $ fib

main = do
  print . solve $ 4000000