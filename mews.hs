import System.Random
import Control.Monad (replicateM)


rollDice :: IO Int
rollDice = getStdRandom (randomR (0,5))

ls = "天地君親師我"

nouns = ["小毛","中毛","大毛","聖人","光使者","天使"]
verbIs = ["運動","呼吸","睡大頭覺","發光","讀書","玩遊戲"]
verbTs = ["嘲笑","幫助","打","踢","求助","尋找"]


main :: IO ()
main  =  do
		[x, x2, y] <- replicateM 3 rollDice
		let msg = case x `mod` 3 of
			0 -> act0
			1 | x /= x2 -> act1
			_ -> act2
			where
			act0 = "本月" ++ (nouns !! x) ++ "很好，每天都" ++ (verbIs !! y)++"\n\n"
			act1 = (nouns !! x) ++ "時常"++(verbTs !! y)++(nouns !! x2)++"\n\n"
			act2 = "非常喜歡和"++(nouns !! x2) ++ "一起" ++ (verbIs !! y)++"\n\n"
		appendFile "mews/output.txt" msg
		if x > 0 || x2 /= 1 then main else putStrLn "done!"
