import ListActions
--exercise 1
run :: IO Int
run = do
  contents <- readFile "vals/values4.txt" 
  let xs = lines contents     
  return (listSum (map check1 (makeLists xs)))

--exercise 2
run2 :: IO Int
run2 = do
  contents <- readFile "vals/values4.txt" 
  let xs = lines contents     
  return (listSum (map check2 (makeLists xs)))

check1 :: [Int] -> Int
check1 (a:b:c:d:_) | a <= c && b >= d = 1
                   | a >= c && b <= d = 1
                   | otherwise = 0

check2 :: [Int] -> Int
check2 (a:b:c:d:_) |  a <= c && b >= c = 1
                   |  a <= d && b >= d = 1
                   |  c <= a && d >= a = 1
                   |  c <= b && d >= b = 1 
                   | otherwise = 0

