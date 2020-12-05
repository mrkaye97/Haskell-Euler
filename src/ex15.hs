-- First, this problem has a closed form solution, which we can get with the following formula:
-- NumPaths = (n + n) choose (n) = (2 * n)! / (n! * n!).
-- This is because the number of northeast lattice paths between (0,0) and (x,y) is equal to (x + y) choose (x).
-- This also happens to be the central column of Pascal's triangle, which is pretty cool and makes sense because entry i in row j of
-- Pascal's triangle is given by j choose i.
-- Let's imagine we don't know about that. Then:
-- this is another one of those DP problems
-- for DP here, we'd like to start with one of three base cases:
-- Case I: 1x1 grid has 2 paths
-- Case II: Nx0 grid has 1 path
-- Case III: Nx1 grid has N+1 paths
-- and then fill out an NxM table where we reduce the problem down to a known one.
-- For example: in the 2x1 case, we have two options: either right or down.
-- If we go right, it's now a 2x0, which we know leaves one path. If we go down, it's a 1x1,
-- which we know leaves two paths. Thus, we have two choices (down and right),
-- and three total possible paths.
-- We can use the same logic for the 2x2 case. If we go down, it's a 1x2, which has three paths
-- (since 1x2 = 2x1). If we go right, it's 2x1, with three paths. Thus, there are six possible paths.
-- For 3x2, we can reduce to 2x2 (6 paths) or 3x1 (4 paths), so there are 10 paths.
-- For 3x3, we can only reduce to 2x3 or 3x2 (both 10 paths), so there are 20 paths, and so on.
import Data.Matrix

setTopRow :: Matrix Int -> Int -> Matrix Int
setTopRow mat c =
  if (c == ncols mat)
    then (updateMat mat 2 1 c)
  else setTopRow (updateMat mat ((ncols mat) - c + 1 + 1) 1 c) (c + 1)

setRightCol:: Matrix Int -> Int -> Matrix Int
setRightCol mat r =
  if (r == 1)
    then (updateMat mat 2 r (ncols mat))
  else setRightCol (updateMat mat (r + 1) r (ncols mat)) (r - 1)

updateMat :: Matrix Int -> Int -> Int -> Int -> Matrix Int
updateMat mat newval r c = setElem newval (r,c) mat

numPaths :: Matrix Int -> Int -> Int -> Int
numPaths mat r c
  | (c == 1) && (r == (nrows mat)) = ((mat ! (r-1, c)) + (mat ! (r, c+1)))
  | (c == (ncols mat)) = numPaths mat r (c-1)
  | (c == 1) = numPaths (updateMat mat ((mat ! (r-1, c)) + (mat ! (r, c+1))) r c) (r + 1) (ncols mat)
  | otherwise = numPaths (updateMat mat ((mat ! (r-1, c)) + (mat ! (r, c+1))) r c) r (c-1)

main = do
  let z = zero 20 20
  let x = setTopRow z 1
  let mat = setRightCol x (nrows x)
  print(numPaths mat 2 (ncols mat))
