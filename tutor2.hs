isP x = foldl (&&) True [x `mod` m > 0 | m <- [2..x-1]]

main = do 
	print $ filter isP [1..1000]