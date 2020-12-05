digits :: Integer -> [Int]
digits n = map (\x -> read [x] :: Int) (show n)

main = do
  print(sum (digits (2^1000)))
