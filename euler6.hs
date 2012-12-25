main = do
	 let res = diff [1..100]
	 print res

diff range = let sumOfSquares = sum $ map (^2) range
                 squareOfSum = (sum range)^2
			  in squareOfSum - sumOfSquares
