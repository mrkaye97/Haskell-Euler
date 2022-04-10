isPalindrome n = n == (read . reverse . show) n
isPalindromeInBinary n = isPalindrome . toBinary $ n

fromDigits = foldl addDigit 0
   where addDigit num d = 10*num + d

-- from SO
toBinary x = fromDigits (reverse $ decToBin' x)
  where
    decToBin' 0 = []
    decToBin' y = let (a,b) = quotRem y 2 in [b] ++ decToBin' a

main = do
  let allPalindromes = filter isPalindrome [1..1000000]
  print . sum $ (filter isPalindromeInBinary allPalindromes)
