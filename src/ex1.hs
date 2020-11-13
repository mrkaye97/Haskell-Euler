calc :: Integer -> Integer 
calc x          
   | x `rem` 5 == 0 = x
   | x `rem` 3 == 0 = x
   | otherwise = 0

main = do 
   let y = map calc [1,2..999] 
   print (sum y)