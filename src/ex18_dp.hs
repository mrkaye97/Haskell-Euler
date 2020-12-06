import Data.List.Split
import Data.Graph.Inductive

raw = "75 95 64 17 47 82 18 35 87 10 20 04 82 47 65 19 01 23 75 03 34 88 02 77 73 07 63 67 99 65 04 28 06 16 70 92 41 41 26 56 83 40 80 70 33 41 48 72 33 47 32 37 16 94 29 53 71 44 65 25 43 91 52 97 51 14 70 11 33 28 77 73 17 78 39 68 17 57 91 71 52 38 17 14 91 43 58 50 27 29 48 63 66 04 68 89 53 67 30 73 16 69 87 40 31 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
triangle = map strToInt (splitOn " " raw)
lhs = scanl (+) 1 [1,2..]
rhs = drop 1 (scanl (+) 0 [1,2..])

getHeight :: Int -> Int
getHeight n =
  if (length triangle) == (rhs !! n)
    then n
  else getHeight (n + 1)

strToInt :: String -> Int
strToInt x = read x :: Int

getRowNum :: Int -> Int -> Int
getRowNum ix r = if ix <= r
  then r
  else (getRowNum (ix - r) (r + 1))

getChildren :: Int -> [Int]
getChildren ix =
  [ix + (getRowNum ix 1), ix + (getRowNum ix 1) + 1]

getRow :: Int -> [Int]
getRow rownum = [lhs !! (rownum-1), ((lhs !! (rownum-1)) + 1)..(rhs !! (rownum-1))]

getMaxChildSum :: Int -> [Int] -> Int
getMaxChildSum ix tr =
  (tr !! (ix - 1)) + maximum (map (\x -> tr !! (x-1)) (getChildren ix))

getMaxChildSumByRow :: Int -> [Int] -> [Int]
getMaxChildSumByRow rowix tr =
  map (\x -> getMaxChildSum x tr) (getRow rowix)

updateTriangle :: Int -> [Int] -> [Int] -> [Int]
updateTriangle rowix newrow tr =
  (take (rhs !! (rowix - 2)) tr) ++ newrow ++ (drop ((rhs !! (rowix - 2)) + length(newrow)) tr)

getMaxTotalSum :: Int -> [Int] -> Int
getMaxTotalSum depth tr =
  if (depth == 1)
    then maximum [(tr !! 0) + (tr !! 1), (tr !! 0) + (tr !! 2)]
  else getMaxTotalSum (depth - 1) (updateTriangle depth (getMaxChildSumByRow depth tr) tr)

main = do
  let height = getHeight 1
  print(getMaxTotalSum (height) triangle)
