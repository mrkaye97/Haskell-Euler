import Data.Char

factorial :: Integer -> Integer
factorial n = product [1..n]

digits :: Integer -> [Int]
digits = map digitToInt . show

digitsum :: [Int] -> Int
digitsum digs = sum digs

main = do
  let x = factorial 100
  print(digitsum . digits . factorial $ 100)
