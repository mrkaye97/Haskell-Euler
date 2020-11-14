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
    let fct = factors 600851475143
    let primefct = filter isPrime fct
    print (maximum primefct)