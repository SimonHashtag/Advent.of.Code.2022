import ListActions
import Data.List as List
import Data.Char as Char

data Operation = In | Out | Size Int
    deriving Show

--exercise 1
run :: IO Int
run = do
  contents <- readFile "vals/values7.txt" 
  let xs = lines contents  
  return (calculateAllUnder100000 (translate xs))

--exercise 2
run2 :: IO Int
run2 = do
  contents <- readFile "vals/values7.txt" 
  let xs = lines contents  
  return (findSmallestWithAtLeastX (translate xs))

calcSizeList :: [Operation] -> [Int] -> [Int]
calcSizeList [] ints = ints
calcSizeList (x:xs) ints = (calcSizeList (xs) ints)

calcSize :: [Operation] -> Int
calcSize ops = calcSizeCounter 0 (getPartInput ops)
    where calcSizeCounter x [] = x
          calcSizeCounter x ((Size n):xs) = calcSizeCounter (x+n) xs
          calcSizeCounter x (_:xs) = calcSizeCounter x xs 

listAllPartLists :: [Operation] -> [[Operation]]
listAllPartLists [] = []
listAllPartLists (In:xs) = [getPartInput (In:xs)] ++ listAllPartLists xs
listAllPartLists (_:xs) = listAllPartLists xs

calcSub100000 :: [Int] -> Int -> Int
calcSub100000 [] num = num
calcSub100000 (x:xs) num = if x <= 100000 then calcSub100000 xs (num+x)
                          else calcSub100000 xs num

findSmallestWithAtLeastX :: [Operation] -> Int
findSmallestWithAtLeastX xs= let sizes = map calcSize (listAllPartLists (xs))
                                 unused = 70000000 - (maximum sizes)
                                 needed = 30000000 - unused
                                    in
                                       minimum (filterBiggerThan sizes needed)


filterBiggerThan :: [Int] -> Int -> [Int]
filterBiggerThan [] _ = []
filterBiggerThan (x:xs) num = if x >= num then x:(filterBiggerThan xs num)
                              else filterBiggerThan xs num

calculateAllUnder100000 :: [Operation] -> Int
calculateAllUnder100000 x = calcSub100000 (map calcSize (listAllPartLists x)) 0

getPartInput :: [Operation] -> [Operation]
getPartInput x = getPartInput' 0 x

getPartInput' :: Int -> [Operation] -> [Operation]
getPartInput' _ [] = []
getPartInput' count (x:ops) = case (x,count) of
             (Size n, 0) -> getPartInput' 0 ops
             (In,c) -> [In] ++ (getPartInput' (c+1) ops)
             (Out,1) -> [Out]
             (Out,c) -> [Out] ++ (getPartInput' (c-1) ops)
             (xi,ci) -> [xi] ++ (getPartInput' ci ops)


getFirstNumber :: String -> Int
getFirstNumber str = helpGetter "" str
    where helpGetter x [] = read x::Int
          helpGetter x (' ':xs) = read x::Int
          helpGetter x (q:qs) = if Char.isDigit q then helpGetter (x ++ [q]) qs
                                else helpGetter x (' ':qs)

translate :: [String] -> [Operation]
translate [] = []
translate (x:xs) = case x of 
  ("$ cd ..") -> [Out] ++ translate xs
  ('$':' ':'c':'d':' ':zs) -> [In] ++ translate xs
  (y:ys) -> if Char.isDigit y then [Size (getFirstNumber (y:ys))] ++ translate xs
            else translate xs