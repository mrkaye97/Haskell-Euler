cartProd :: [Int] -> [Int] -> [(Int, Int)]
cartProd a b = [ (x,y) | x <- a, y <- b ]

mgrn :: (Int, Int) -> Bool
mgrn (m,n) = if m > n
  then True
  else False

-- getting pythagorean triples is simple, if we have 2 integers m and n such that m > n
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
  let x = (filter canWork (filter mgrn (cartProd [1,2..1000] [1,2..1000])))
  let a = (map getA x) !! 0
  let b = (map getB x) !! 0
  let c = (map getC x) !! 0
  print(a, b, c)
  print(a + b + c)
