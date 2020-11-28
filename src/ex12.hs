--this solution is really inefficient because getDivisors is repeating a lot of work
--for each recursion, I need to recalculate the divisors of the new number
--I'll come back to this when I learn more Haskell
getAnswer :: Int -> Int -> Int
getAnswer current_triangle current_index = if (((length (getDivisors current_triangle)) + 1) == 500)
  then current_triangle
  else getAnswer (getNextTriangle current_triangle current_index) (current_index + 1)

getNextTriangle :: Int -> Int -> Int
getNextTriangle curr_triangle curr_index = curr_triangle + curr_index + 1

getDivisors :: Int -> [Int]
getDivisors num = filter (\x -> (num `rem` x) == 0) [1,2..(num `div` 2)]

main = do
  print(getAnswer 1 1)
