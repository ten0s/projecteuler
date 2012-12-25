main = do
	 let res = let seq = show $ sum [x^x | x <- [1..1000]]
         	       len = length seq
			   in drop (len - 10) seq
	 print res
