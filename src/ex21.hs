properDivisors :: Int -> [Int]
properDivisors n = filter (\x -> n `mod` x == 0) [1..(n-1)]

properDivisorSum :: Int -> Int
properDivisorSum n = sum (properDivisors n)

isAmicablePair :: Int -> Int -> Bool
isAmicablePair n1 n2 = if properDivisorSum n1 == n2 then True else False

getAmicableSum :: Int -> [Int] -> Int
getAmicableSum n propdivs = if ((isAmicablePair (propdivs !! n) n) && ((propdivs !! n) /= n)) then n else 0

main = do
  let max_n = 10000
  let propdivs = map properDivisorSum [0..max_n]
  print(sum(map (\x -> getAmicableSum x propdivs) [0..max_n]))
