import Data.Set (Set)
import qualified Data.Set as Set

isDividedBy num den = (num `mod` den) == 0
dividesN n = isDividedBy n
properDivisors n = filter (dividesN n) [1..(n-1)]
properDivisorSum n = (sum . properDivisors) n
isAbundant n = n < properDivisorSum n

isNotSumOfTwoAbundant n =
  Set.null (Set.intersection (Set.fromList eligibleAbundant) (Set.fromList possibilities))
  where
    allAbundant = filter isAbundant [1..]
    eligibleAbundant = takeWhile (< n) allAbundant
    possibilities = map (\x -> n - x) eligibleAbundant

main = do
  let sums = sum (filter isNotSumOfTwoAbundant [1..28123])
  print(sums)
