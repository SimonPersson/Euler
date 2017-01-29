data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
                deriving (Eq, Enum, Show, Ord)

data Date = Date { year :: Int , month :: Month, day :: Int } deriving (Show, Eq, Ord)

nextDate (Date y m d) 
  | d + 1 == daysInM = nextMonth
  | otherwise = Date y m (d+1)
    where 
  daysInM | m == Feb = if leapY then 29 else 28
          | otherwise = if m `elem` [Sep, Apr, Jun, Nov] then 30 else 31
  leapY = (rem y 100 /= 0 && rem y 4 == 0) || (rem y 400 == 0)
  nextMonth = case m of Dec -> Date (y+1) Jan 0
                        otherwise -> Date y (succ m) 0

main = do 
  print . length 
        . takeWhile (<= Date 2001 Jan 0) 
        . filter (>= Date 1901 Jan 0)
        . map snd . filter ((==) 7 . fst) . filter ((==) 0 . day . snd)
        . zip (cycle [1 .. 7]) $ iterate nextDate $ Date 1900 Jan 0
