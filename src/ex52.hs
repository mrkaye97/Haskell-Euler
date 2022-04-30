import Data.List
import Data.Maybe

digits n = map (\x -> read [x] :: Int) (show n)

sameDigits x y = sort (digits x) == sort (digits y)

allSameDigits x = all (== True) (map (\y -> sameDigits y first) x)
  where
    first = x !! 0

multiples x = [x, 2*x, 3*x, 4*x, 5*x, 6*x]
allMultiplesSameDigits x = allSameDigits (multiples x)

getMaxEligibleX numDigits = floor((10^(numDigits) - 1) / 6)
getMinEligibleX numDigits = 10^(numDigits-1)

listEligibleNums n = [lo..hi]
  where
    hi = getMaxEligibleX n
    lo = getMinEligibleX n

main = do
  let proposals = concat $ map listEligibleNums [1..]
  let ix = findIndex (==True) (map allMultiplesSameDigits proposals)
  print (proposals !! (fromJust ix))
