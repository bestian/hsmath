import System.Random
import Control.Monad (replicateM)


rollDice :: IO Int
rollDice = getStdRandom (randomR (0,9))


nouns = ["小毛","小明","小華","聖人","光使者","小蟲","天使","鬼豬","機器人","神探福爾摩斯"]
verbIs = ["運動","呼吸","睡大頭覺","發光","讀書","玩遊戲","休息","打坐","跳啊跳", "飛行"]
verbTs = ["嘲笑","幫助","打","踢","求助", "尋找","收拾","資助","拒絕","親"]
numbers = ["零","一","二","三","五","五十","一百","一零二四","可數無限多","不可數無限多"]


main :: IO ()
main  =  do
		[x, x2, y, z] <- replicateM 4 rollDice
		let msg = case x `mod` 6 of
			0 -> act0
			1 | x /= x2 -> act1
			2 | x /= x2 -> act2
			3 -> act3
			_ -> act4
			where
			act0 = "本月" ++ (nouns !! x) ++ "很好，每天都" ++ (verbIs !! y) ++ (numbers !! z)++"次\n"
			act1 = (nouns !! x) ++ "時常"++(verbTs !! y)++(nouns !! x2)++"，對此"++(nouns !! x2)++"不予理會\n"
			act2 = (nouns !! x) ++ "非常喜歡和"++(nouns !! x2) ++ "一起" ++ (verbIs !! y)++"\n"
			act3 = (nouns !! x) ++ "是今天的主角"++"\n"
			act4 = "經過" ++ (numbers !! z) ++"次體驗後，"++(nouns !! x2) ++ "宣示再也不" ++ (verbTs !! y)++"別人了"++"\n"
		appendFile "mews/output.txt" msg
		if x > 0 || x2 /= 1 then main else putStrLn "done!"
