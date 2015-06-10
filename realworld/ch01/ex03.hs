--file: ch01/ex03.hs

main = interact wordCount
    where wordCount input = (show $ (length (words input))) ++ "\n"