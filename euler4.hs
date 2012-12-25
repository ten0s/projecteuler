main = do
	 let res = maximum [x*y | x <- [999,998..100],
	 	 	   		   		  y <- [999,998..100],
							  isPalindrome (show (x*y))]
	 print res

isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = xs == reverse xs
