main = do
	--a + b + c == 1000 => c == 1000 - a - b
	--a^2 + b^2 == c^2  => 500000 = 1000 * (a + b) - a * b
	let res = let (a, b, c) = head [(a, b, 1000 - a - b) | a <- [1..998], b <- [1..998], 1000 * (a + b) - a * b == 500000]
       	  	  	in a * b * c
	print res
