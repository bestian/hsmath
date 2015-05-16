

mean :: [Float] -> Float
mean a = sum a / (fromIntegral.length) a


fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)



fibo 0 = [1]
fibo 1 = [1,1]
fibo n = map (\x -> fib x) [1..n]