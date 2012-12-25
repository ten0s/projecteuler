main = do
	 let res = (sum . map charToInt . show . fac) 100
	 print res

charToInt :: Char -> Int
charToInt c = read [c]

fac :: (Integral a) => a -> a
fac n = product [1..n]
