
summ f xs = sum $ map f xs
prod f xs = product $ map f xs


paskalT n = map paskal [0..n]


paskal 0 = [1]
paskal 1 = [1,1]
paskal 2 = [1,2,1]
paskal n = map (\x -> comb n x) [0..n]

perm n x = product [n-x+1..n]
comb n x = (fact n) `div` ((fact x) * fact (n-x))

fact 0 = 1
fact n = product [1..n]


main = do
	print $ summ (*2) [1..10] == 2+4+6+8+10+12+14+16+18+20	{- True -}
	print $ prod (+1) [1..4] == 2*3*4*5	{- True -}
	print $ perm 5 3 == 5*4*3
	print $ comb 5 3 == 5*4*3 `div` 2	
	print $ paskalT 6