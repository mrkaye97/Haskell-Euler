fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

main = do
    let fibnums = map fib [1,2..33]
    let evenfibs = filter even fibnums
    print(sum evenfibs)