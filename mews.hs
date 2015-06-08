import System.Random


rollDice :: IO Int
rollDice = getStdRandom (randomR (0,5))

ls = "天地君親師我"

nouns = ["小毛","中毛","大毛","聖人","光使者","天使"]
verbIs = ["運動","呼吸","睡大頭覺","發光","讀書","玩遊戲"]
verbTs = ["嘲笑","幫助","打","踢","求助","尋找"]


main :: IO ()
main  =  do 
		x <- rollDice
		x2 <- rollDice
		y <- rollDice
		if (x == 0 || x == 1) then 			-- Fix me
			appendFile "mews/output.txt" ("本月" ++ (nouns !! x) ++ "很好，每天都" ++ (verbIs !! y)++"\n\n")	
			else
			if ((x == 2 || x == 3) && (x2 /= x)) then
				appendFile "mews/output.txt" ((nouns !! x) ++ "時常"++(verbTs !! y)++(nouns !! x2)++"\n\n")
				else appendFile "mews/output.txt" ((nouns !! x) ++ "非常喜歡和"++(nouns !! x2) ++ "一起" ++ (verbIs !! y)++"\n\n")			
		if (x > 0 || x2 /= 1) then -- Fix me
			do main
			else
				print "done!"

