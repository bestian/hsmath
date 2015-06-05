
{-算術平均-}
mean :: [Float] -> Float
mean a = sum a / (fromIntegral.length) a


{-   copy from web  -}
n `nthRoot` x = fst $ until (uncurry(==)) (\(_,x0) -> (x0,((n-1)*x0+x/x0**(n-1))/n)) (x,x/n)
{-   copy from web  -}


{-幾何平均-}
geo :: [Float] -> Float
geo a = ((fromIntegral.length) a) `nthRoot` (foldl (*) 1 a)


{-調合平均-}

harmony :: [Float] -> Float
harmony a = 1 / (mean (turn a))
	where turn a = map (\x -> 1/x) a


{-非波那其數列-}

fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibo 0 = [1]
fibo 1 = [1,1]
fibo n = map (\x -> fib x) [1..n]


main                    :: IO ()
main                    =  do print $ fibo 3