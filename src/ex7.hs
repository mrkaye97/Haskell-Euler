-- the filter() checks if the factors of each number are just 1 and the number
-- the . works like the %>% in R, so this filter does primeFactors then tail of that and then null of that
-- it's like function composition
primes = 2 : filter (null . tail . primeFactors) [3,5..]

primeFactors :: Integer -> [Integer]
primeFactors n = factor n primes
  where
    factor n (p:ps)
        | p*p > n        = [n]
        | n `mod` p == 0 = p : factor (n `div` p) (p:ps)
        | otherwise      =     factor n ps


main = do
    print(primeFactors [3,5,6])
    print(primes !! 10000)
