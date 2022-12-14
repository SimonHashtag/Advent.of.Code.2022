import Data.List as List
import ListSum
--exercise 1
run :: IO Int
run = do
  contents <- readFile "vals/values1.txt" 
  let xs = lines contents     
  return (calcMax xs 0 0)

--exercise 2
run2 :: IO Int
run2 = do
  contents <- readFile "vals/values1.txt" 
  let xs = lines contents      
  return (calcMax3 xs 0 [0,0,0])


--calculate max sum of input List
calcMax :: [String] -> Int -> Int -> Int
calcMax [] curr max = case curr<=max of
    False  -> curr
    True -> max
calcMax ("":xs) curr max = case curr<=max of
    False  -> calcMax xs 0 curr
    True -> calcMax xs 0 max
calcMax (x:xs) curr max = calcMax xs (curr+(read x::Int)) max

--calculate largest 3 from input list
calcMax3 :: [String] -> Int -> [Int] -> Int
calcMax3 [] curr max = listSum (swap curr max)
calcMax3 ("":xs) curr max = calcMax3 xs 0 (swap curr max)
calcMax3 (x:xs) curr max = calcMax3 xs (curr+(read x::Int)) max


--swap x for the smallest element in the list if x is larger than it.
swap :: Int -> [Int] -> [Int]
swap x xs = let sortXs = List.sort xs
    in 
        case sortXs of 
            (y:ys) -> if x > y then (x:ys)
                      else sortXs



