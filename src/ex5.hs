-- this is the smallest possible answer, and all possible answers must be multiples of it
-- we know it's the smallest possible, because 9699690 = 2 * 3 * 5 * 7 * 11 * 13 * 17 * 19
-- thus, any number divisible by everything between 1 and 20 must also be divisible by
-- 9699690 (since the prime factorization of whatever the solution is must incluse each of
--  the primes between 1 and 20 at least once, and if any are included multiple times, then 
--  the solution will be a multiple of 9699690)
minimum_prime_factor = 9699690

check :: Integer -> Integer -> Bool 
check x 1 = True
check x n = 
    if x `rem` n == 0 then check x (n - 1)   
    else False

calc :: Integer -> Integer
calc x = 
    if check x 20 == True then x
        else calc (x + minimum_prime_factor)


main = do
    print(calc minimum_prime_factor)

