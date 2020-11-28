-- getting pythagorean triples is simple
-- if we have 2 integers m and n such that m > n,
-- then we can find a pythagorean triple (a,b,c)
-- s.t. a = m^2 - n^2, b = 2 * m * n, c = m^2 + n^2
-- thus, my approach is to first get the cartestian product of
-- M = [1,2..1000], N = [1,2..1000]
-- and then  filter than list down s.t. m > n
-- and then get the pythagorean triple corresponding to that (m,n) pair
-- and finally, to check if a + b + c == 1000

cartProd :: [Int] -> [Int] -> [(Int, Int)]
cartProd a b = [ (x,y) | x <- a, y <- b ]

mgrn :: (Int, Int) -> Bool
mgrn (m,n) = if m > n
  then True
  else False

canWork :: (Int, Int) -> Bool
canWork (m,n) = if (m + n < 1000) &&
  (((getA (m, n)) + (getB (m,n)) + (getC (m,n))) == 1000)
  then True
  else False

getA :: (Int, Int) -> Int
getA (m, n) = m^2 - n^2

getB :: (Int, Int) -> Int
getB (m, n) = 2 * m * n

getC :: (Int, Int) -> Int
getC (m, n) = m^2 + n^2

main = do
  let x = (filter canWork (filter mgrn (cartProd [1,2..1000] [1,2..1000]))) !! 0
  let a = getA x
  let b = getB x
  let c = getC x
  print(a, b, c)
  putStrLn $ "Triple = " ++ show (a,b,c)
  putStrLn $ "Sum = " ++ show (a+b+c)
  putStrLn $ "Product = " ++ show (a*b*c)
