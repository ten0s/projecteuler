main = do
	 let res = sum $ map charToInt $ show $ 2^1000
	 print res

charToInt :: Char -> Int
charToInt c = read [c]
