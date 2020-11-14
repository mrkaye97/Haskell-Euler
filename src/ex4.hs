palindrome :: Integer -> Bool
palindrome x = reversal x == x

reversal :: Integral a => a -> a
reversal = go 0
  where go a 0 = a
        go a b = let (q,r) = b `quotRem` 10 in go (a*10 + r) q

palProd :: Integer -> Integer -> Integer
palProd x y = 
    if palindrome (x * y) then x*y
    else 0

main = do
    print(maximum ([ palProd x y  | x <- (filter (\x -> x `rem` 11 == 0)[100..999]), y <- [100..999]]))