



main :: IO ()
main = do  
    putStrLn "Hello, what's your name?"  
    name <- getLine  
    putStrLn ("Hey " ++ name ++ ", you rock!")  
    if (length name > 5) then biggie
    	else smally


biggie :: IO()
biggie = do
	putStrLn "how long your name is!"


smally :: IO()
smally = do
	putStrLn "how short your name is!"
