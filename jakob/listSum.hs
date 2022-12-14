module ListSum where
--sum of int list
listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + listSum xs
