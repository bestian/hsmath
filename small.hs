

o = map (\x->foldl (*) 1 [1..x]) [1..10]



main :: IO ()
main  =  do 
		f <- readFile "./input.txt"
		writeFile "./output.txt" (show o)