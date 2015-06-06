
{- 3x + 15 = 5 -}



solve1 :: String -> String
solve1 f = case a2 of
		[] -> show $ (0 + read (tail b2) + read (tail c2)) / read tt 
		_ -> show $ (0 + read (tail b2) - read (tail a2)) / read tt
		where
			(b1,b2) = break (\x -> x == '=') f
			(a1,a2) = break (\x -> x == '+') b1
			(c1,c2) = break (\x -> x == '-') a1
			(f1,f2) = break (\x -> x == 'x') f
			tt = getTail f1
			getTail f1 = case f1 of 
				[] -> "1"
				_ -> f1



checklast :: String -> Int
checklast f = case any (\x -> or [x == '+',x == '-']) f of
		True -> 1
		False -> 0


solve :: String -> String
solve f = case checklast f of
			0 -> f ++ "\nAnswer: " ++ x ++ " = " ++ [(reverse f) !! 0]
			1 -> f ++ "\nAnswer: " ++ x ++ " = " ++ solve1 f
			_ -> f ++ "\nI'm not sure..."
		where
			x = "x"

main :: IO ()
main  =  do 
		f <- readFile "leq/input.txt"
		writeFile "leq/output.txt" (solve f)