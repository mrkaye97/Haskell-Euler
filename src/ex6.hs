square :: Integer -> Integer
square x = x * x

sumSq :: [Integer] -> Integer
sumSq x = sum(map square x)

sqSum :: [Integer] -> Integer
sqSum x = square (sum x)

diff :: Integer -> Integer -> Integer
diff a b = b - a

main = do
    print(diff (sumSq [1..100]) (sqSum [1..100]))
    