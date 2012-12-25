main = do
	 let res = length [d | d@(date, dayOfWeek) <- twentiethCentury, day date == 1, dayOfWeek == Sunday]
	 print res

twentiethCentury = takeWhile (\(date,_) -> date < Date {day = 31, month = December, year = 2000})
                 $ dropWhile (\(date,_) -> date < Date {day = 1, month = January, year = 1901})
                 $ datesFrom Date {day = 1, month = January, year = 1900}

datesFrom date = zip (iterate nextDate date)
                     (cycle [Monday .. Sunday])

data Day = Monday
         | Tuesday
         | Wednesday
         | Thursday
         | Friday
         | Saturday
         | Sunday
           deriving (Eq, Ord, Show, Bounded, Enum)

data Month = January
           | February
           | March
           | April
           | May
           | June
           | July
           | August
           | September
           | October
           | November
           | December
             deriving (Eq, Ord, Show, Bounded, Enum)

data Date = Date { year :: Int
                 , month :: Month
                 , day :: Int
                 } deriving (Eq, Ord, Show)

nextDate :: Date -> Date
nextDate date = if d == daysInMonthYear m y
                then if m == December
                     then date { day = 1, month = January, year = y + 1 }
                     else date { day = 1, month = succ m }
                else date { day = d + 1 }
    where d = day date
          m = month date
          y = year date

daysInMonthYear January   _    = 31
daysInMonthYear February  year = if leapYear year then 29 else 28
daysInMonthYear March     _    = 31
daysInMonthYear April     _    = 30
daysInMonthYear May       _    = 31
daysInMonthYear June      _    = 30
daysInMonthYear July      _    = 31
daysInMonthYear August    _    = 31
daysInMonthYear September _    = 30
daysInMonthYear October   _    = 31
daysInMonthYear November  _    = 30
daysInMonthYear December  _    = 31

leapYear :: Int -> Bool
leapYear year = if year `mod` 4 == 0
                then if year `mod` 100 == 0
                     then if year `mod` 400 == 0
                          then True
                          else False
                     else True
                else False
