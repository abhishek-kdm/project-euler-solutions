-- answer: 25164150

solve :: Int -> Int
solve n = (-) ((^) (sum ns) 2) (sum . map (flip (^) 2) $ ns)
  where
    ns = [1..n]

main = do
  print . solve $ 100