-- this is ridiculously inefficeint, because I need to calculate the
-- factors of every number
-- in python, I could do this with DP. make a table of primes and then check if those numbers
-- divise each new number, but I'm not sure how to do something like
-- that in Haskell yet

intSquareRoot :: Integer -> Integer
intSquareRoot n = ceiling (sqrt (fromIntegral n))

factors :: Integer -> [Integer]
factors y =
    [ (x, y `div` x) | x <- [1..k], y `mod` x == 0] >>= \(x,y) -> [x,y]
    where k = intSquareRoot y

isPrime :: Integer -> Bool
isPrime n =
    if length (factors n) == 2 then True
    else False


main = do
  print(sum (filter isPrime [1,2..2000000]))
