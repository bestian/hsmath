--file: ch01/ex04.hs

count ls = sum $ map (\s -> length s) ls

main = interact wordCount
    where wordCount input = (show $ (count (words input))) ++ "\n"