module ListActions where
import Data.Char as Char
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
