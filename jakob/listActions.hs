module ListActions where
import Data.Char as Char
import Data.List as List
--sum of int list
listSum :: [Int] -> Int
listSum [] = 0
listSum (x:xs) = x + listSum xs

makeLists :: [String] -> [[Int]]
makeLists str = map (\x -> map (\y -> read y :: Int) (splitter (numbersInString x))) str

numbersInString :: String -> String
numbersInString [] = []
numbersInString (x:xs) | Char.isDigit x = x:(numbersInString xs)
                       | otherwise = "_"++(numbersInString xs)

firstSplit :: String -> String
firstSplit ('_':xs) = ""
firstSplit (x:xs) = x:(firstSplit xs)
firstSplit "" = ""

removeFirst :: String -> String
removeFirst ('_':xs) = xs
removeFirst (x:xs) = removeFirst xs
removeFirst _ = "" 

splitter :: String -> [String]
splitter "" = []
splitter x = [firstSplit x] ++ (splitter (removeFirst x))

removeEmpties :: [String] -> [String]
removeEmpties [] = []
removeEmpties ("":xs) = removeEmpties xs
removeEmpties (x:xs) = x:(removeEmpties xs)

extractInts :: [String] -> [[Int]]
extractInts xs = (map (\x -> map (\y -> read y :: Int) (removeEmpties (splitter (numbersInString x)))) xs)

revert :: [a] -> [a]
revert [] = []
revert (x:xs) = (revert xs) ++ [x]

removeFirstN :: String -> Int -> String
removeFirstN "" _ = ""
removeFirstN x 0 = x 
removeFirstN (x:xs) n = removeFirstN xs (n-1)

allDifferent :: Eq a => [a] -> Bool
allDifferent x = if ((length x) == (length (allElems x []))) then True
                 else False

allElems :: Eq a => [a] -> [a] -> [a]
allElems [] elems = elems
allElems (x:xs) elems = if not (x `elem` elems) then allElems xs (x:elems)
                        else allElems xs elems

nTimes :: String -> Int-> String
nTimes a b = nTimesString a b ""
    where
        nTimesString _ 0 x = x
        nTimesString c n x = nTimesString c (n-1) (c ++ x)

zipString :: [String] -> String
zipString [] = ""
zipString (x:xs) = x ++ (zipString xs)

addUnique :: Eq a => a -> [a] -> [a]
addUnique item list = if item `elem` list then list
                      else (item:list)

getNth :: [a] -> Int -> a
getNth (x:[]) _= x
getNth (x:xs) 1= x
getNth (x:xs) n = getNth xs (n-1)

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn _ [] = []
splitOn x xs = case break (== x) xs of
                 (ys, []) -> [ys]
                 (ys, _:zs) -> ys : splitOn x zs