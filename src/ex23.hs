#!/usr/bin/env stack
-- stack --resolver lts-16.21 script --package containers
-- https://www.fpcomplete.com/haskell/tutorial/stack-script/
import Data.Set (Set)
import Data.List
import qualified Data.Set as Set

divisorPair den num = if (num `mod` den) == 0 then (nub [den, num `div` den]) else []
properDivisors n = filter (\x -> x /= n) (concat (map (\x -> divisorPair x n) [1..((floor . sqrt . fromIntegral) n)]))
properDivisorSum n = (sum . properDivisors) n
isAbundant n = n < properDivisorSum n

isNotSumOfTwoAbundant n abundantNums =
  Set.null (Set.intersection (Set.fromList eligibleAbundant) (Set.fromList possibilities))
  where
    eligibleAbundant = takeWhile (\x -> x < n && x > 0) abundantNums
    possibilities = filter (\x -> x > 0 && x /= n) (map (\x -> n - x) eligibleAbundant)

main = do
  let n = 28123
  let abundantNums = filter isAbundant [1..n]
  let notSumOfTwoAbundant = filter (\x -> isNotSumOfTwoAbundant x abundantNums) [1..n]
  print (sum notSumOfTwoAbundant)
