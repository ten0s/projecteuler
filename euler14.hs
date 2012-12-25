main = do
	 let res = foldl (\acc@(_,len) x -> let len' = (length $ collatzChain x)
	 	                                in if len < len' then (x, len') else acc)
		(1, 1) [2..1000000]
	 print res

collatzChain :: (Integral a) => a -> [a]
collatzChain 1 = [1]
collatzChain n
	| even n 	= n : collatzChain (n `div` 2)
	| otherwise = n : collatzChain (n * 3 + 1)
