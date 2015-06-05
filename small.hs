

enc :: Char -> Char
enc 'l' = 'i'
enc	chr = chr 

encode :: String -> String
encode s = map (\c -> enc c) s





main :: IO ()
main  =  do 
		f <- readFile "./input.txt"
		writeFile "./output.txt" (f++"\n\n"++(show.length) f++"字")
		print $ lines (f++(show.length) f)