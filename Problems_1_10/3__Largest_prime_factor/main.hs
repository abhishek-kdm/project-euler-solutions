-- answer: 6857

prime :: (Int, Int) -> (Int, Bool)
prime (x, y)
  | isPrime x = (x, True)
  | otherwise = (x, False)
  where
    isPrime n = length [x | x <- [2..(floor . sqrt . fromIntegral $ n)], n `mod` x == 0] == 0

solve :: Int -> Int
solve n = fst
        . head
        . dropWhile (not . snd)
        . map prime
        . filter (\(x, y) -> y == 0)
        . map (divMod n) $ [2..]


main = do
  print . solve $ 600851475143