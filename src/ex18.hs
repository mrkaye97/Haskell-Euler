import Data.List.Split
import Data.Graph.Inductive

raw = "75 95 64 17 47 82 18 35 87 10 20 04 82 47 65 19 01 23 75 03 34 88 02 77 73 07 63 67 99 65 04 28 06 16 70 92 41 41 26 56 83 40 80 70 33 41 48 72 33 47 32 37 16 94 29 53 71 44 65 25 43 91 52 97 51 14 70 11 33 28 77 73 17 78 39 68 17 57 91 71 52 38 17 14 91 43 58 50 27 29 48 63 66 04 68 89 53 67 30 73 16 69 87 40 31 04 62 98 27 23 09 70 98 73 93 38 53 60 04 23"
trianglestr = splitOn " " raw
triangle = map strToInt (splitOn " " raw)

genLNodes :: [LNode String]
genLNodes = zip [1,2..(length triangle)] (map show [1,2..(length triangle)])

genLEdges :: [LEdge Int]
genLEdges = concat[map (\x -> getEdge x 1) [1,2..((length triangle) - 15)], map (\x -> getEdge x 2) [1,2..((length triangle) - 15)]]

mygraph :: Gr String Int
mygraph = mkGraph genLNodes genLEdges

strToInt :: String -> Int
strToInt x = read x :: Int

getRowNum :: Int -> Int -> Int
getRowNum ix r = if ix <= r
  then r
  else (getRowNum (ix - r) (r + 1))

getEdge :: Int -> Int -> (Int, Int, Int)
getEdge ix num = (ix, ix + (getRowNum ix 1) + num - 1, (triangle !! (ix-1)))


main = do
--  print(genLNodes)
--  print(genLEdges)
--  print(mygraph)
--  print(spLength 1 120 mygraph)
  let topsorted = topsort mygraph
  print(topsorted)
