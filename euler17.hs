import Data.Char (isLetter)

main = do
	 let res = length $ filter isLetter $ concatMap spellNumber [1..1000]
	 print res

spellNumber :: Int -> String
spellNumber n
    | n < 0              = "minus" ++ " " ++ spellNumber (-n)
    | n >= 0  && n <= 19 = cardinals0 !! n
    | n >= 20 && n <= 99 = let (d, m) = n `divMod` 10
                               big = cardinals1 !! d
                           in if m == 0 then big else big ++ "-" ++ cardinals0 !! m
    | n >= 100 && n <= 999 = let (d, m) = n `divMod` 100
                                 big = spellNumber d ++ " " ++ "hundred"
                             in if m == 0 then big else big ++ " and " ++ spellNumber m
    | n >= 1000 && n <= 999999 = let (d, m) = n `divMod` 1000
                                     big = spellNumber d ++ " " ++ "thousand"
                                 in if m ==0 then big else big ++ " " ++ spellNumber m
    where cardinals0 = ["zero", "one", "two", "three", "four", "five", "six", "seven", "eight", "nine",
                        "ten", "evelen", "twelve", "thirteen", "fourteen", "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
          cardinals1 = ["", "", "twenty", "thirty", "forty", "fifty", "sixty", "seventy", "eighty", "ninety", "eighty", "ninety"]
