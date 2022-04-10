import Data.Char (digitToInt)
import Fraction

toDigits = map digitToInt . show

cancelDigits num den
  | ((numDigs !! 1) == 0) && ((denDigs !! 1) == 0) = [num, den]
  | numDigs == denDigs = [num, den]
  | (numDigs !! 0) == (denDigs !! 0) = [(numDigs !! 1), (denDigs !! 1)]
  | (numDigs !! 0) == (denDigs !! 1) = [(numDigs !! 1), (denDigs !! 0)]
  | (numDigs !! 1) == (denDigs !! 0) = [(numDigs !! 0), (denDigs !! 1)]
  | (numDigs !! 1) == (denDigs !! 1) = [(numDigs !! 0), (denDigs !! 0)]
  | otherwise = [num, den]
  where
    numDigs = toDigits num
    denDigs = toDigits den

isCurious num den
  | (withCancelled !! 1) == 0 = False
  | num > den = False
  | otherwise = (newQuotient == origQuotient) && (newNum /= num)
  where
    withCancelled = cancelDigits num den
    origQuotient = (fromIntegral num) / (fromIntegral den)
    newNum = withCancelled !! 0
    newDen = withCancelled !! 1
    newQuotient = (fromIntegral newNum) / (fromIntegral newDen)

generatePair num den = if (isCurious num den) then [num, den] else []

checkAllPairs n = map (\x -> generatePair n x) [10..99]

compactPairs pairs = filter (not . null) pairs

main = do
  let pairs = compactPairs (map (compactPairs . checkAllPairs) [10..99])
  let unpackedPairs = map (\x -> (x !! 0)) pairs

  print ((den . product) (map (\x -> ((//) (toInteger (x !! 0)) (toInteger (x !! 1)))) unpackedPairs))
