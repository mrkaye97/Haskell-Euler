-- This is another one where it seems like a DP solution makes sense
-- The way I'm thinking about this is that the Collatz sequences are "memoryless".
-- In another sense, they're Markov Chains. This means that once we've seen
-- Collatz(13), then we shouldn't recompute it again if any other sequence hits 13
-- If we make a table of K-V pairs where the keys are the start numbers, then we
-- Can get O(1) lookups and not need to recalculate any sequences, which should
-- be a significant time saver

import Data.Function (fix)

getCollatz :: Int -> [Int] -> [Int]
getCollatz start l =
  if (start == 1)
    then
      l ++ [1]
  else if (even start) == True
    then getCollatz (start `div` 2) (l ++ [start])
  else getCollatz (3 * start + 1) (l ++ [start])

main = do
  print(getCollatz 13 [])
  print(maximum (map length (map (\x -> getCollatz x []) [1,2..1000000])))
