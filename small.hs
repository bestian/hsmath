check :: String -> String
check s = case compare (length s) 50 of
	LT -> "."
	EQ -> "..."
	GT -> "!"

main :: IO ()
main  =  do 
		f <- readFile "./input.txt"
		writeFile "./output.txt" (f++"\n\n"++(show.length) f++check f++"字")
		print $ lines (f++(show.length) f)