main = do
	 let res = sum $ filter even $ takeWhile (<=4000000) fibs
	 print res

fib n = fibs !! n
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)
