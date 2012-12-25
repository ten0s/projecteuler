import Data.List (sortBy)
import Data.Function (on)

--
-- Even though this gives the right answer I'm not happy with the solution at all :(
-- http://en.wikipedia.org/wiki/Repeating_decimal
--
main = do
	 let (res, _) = let m = 999
	                    n = zip [2..m] (map (cycleDecimals [] 1) [2..m])
     			   in head $ sortBy (flip compare `on` length.snd) n
	 print res

cycleDecimals xs x y
    | x == 0 = []
    | x == y = []
    | x `elem` xs = []
    | otherwise = let (d, m) = (x*10) `divMod` y
                  in if m == 0
                     then [d]
                     else d : cycleDecimals (x : xs) m y
