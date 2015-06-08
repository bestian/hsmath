



main :: IO ()
main = do  
    putStrLn "\nHello, what's your name?"  
    name <- getLine  
    putStrLn ("\nHey " ++ name ++ ", you rock!")  
    if (length name > 5) then biggie
    	else smally
    putStrLn "\nagain? y/n/q"  
    coco <- getChar  
    case coco of 'Y' -> main
    	         'n' -> byebye
    	         'q' -> byebye
    	         _ -> main 


biggie :: IO()
biggie = do
	putStrLn "\nhow long your name is!"


smally :: IO()
smally = do
	putStrLn "\nhow short your name is!"


byebye :: IO()
byebye = do
	putStrLn "\nbyebye~"