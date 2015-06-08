
withIndex xs = zip xs [0..]

bingbang = map makeBB (withIndex [0..20])

makeBB (x,k) = if (k `mod` 3 == 0 && k `mod` 5 == 0) then "BINGBANG"
				else if (k `mod` 3 == 0) then "BING"
					else if (k `mod` 5 == 0) then "BANG"
						else show x
				
main = do
	print bingbang