
apply :: (a -> a) -> Int -> (a -> a)
apply f n = foldl (.) id [f | _ <- [1..n]]

enc :: Char -> Char
enc 'z' = 'a'
enc 'Z' = 'A'
enc n = succ n

dec :: Char -> Char
dec 'a' = 'z'
dec 'A' = 'Z'
dec n = pred n 

encode :: String -> String
encode s = map (apply enc 3) s

decode :: String -> String
decode s = map (apply dec 3) s


main :: IO()
main = do 
        print "[E]ncode/[d]code?"
        coco <- getChar
        case coco of
            'e' -> ennn
            'd' -> deee
            _ -> main


ennn :: IO()
ennn = do    
    f <- readFile "tutor5/input.txt"
    writeFile "tutor5/output.txt" (encode f)

deee :: IO()
deee = do 
        f <- readFile "tutor5/output.txt"
        writeFile "tutor5/output2.txt" (decode f) 