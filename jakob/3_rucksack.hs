import ListSum
--exercise 1
run :: IO Int
run = do
  contents <- readFile "vals/values3.txt" 
  let xs = lines contents     
  return (calcPrio xs)

--exercise 2
run2 :: IO Int
run2 = do
  contents <- readFile "vals/values3.txt" 
  let xs = lines contents     
  return (calcTriplePrio xs)

seperateListLeft :: (Eq a) => Int -> [a] -> [a]
seperateListLeft n (x:xs) = if (length xs) <= n then []
    else x:(seperateListLeft (n+1) xs)

seperateListRight :: (Eq a) => Int -> [a] -> [a]
seperateListRight n (x:xs) = if (length xs) <= n then (x:xs)
    else (seperateListRight (n+1) xs)

makeListTriples :: [String] -> [(String,String,String)] 
makeListTriples (x:y:z:[]) = [(x,y,z)]
makeListTriples (x:y:z:xs) = (x,y,z):makeListTriples xs

singleIntersection ::(Eq a) => ([a],[a]) -> a
singleIntersection ((x:xs),y) = if x `elem` y then x 
    else singleIntersection (xs,y)

intersection ::(Eq a) => ([a],[a]) -> [a]
intersection ([],y) = []
intersection ((x:xs),y) = if x `elem` y then x:(intersection (xs,y))
    else intersection (xs,y)

tripleIntersection :: (Eq a) => ([a], [a], [a]) -> a
tripleIntersection (x,y,z) = singleIntersection (intersection (x,y), z)

prioValue :: Char -> Int
prioValue x | 97 <= (fromEnum x) && (fromEnum x) <= 122 = (fromEnum x)-96
            | otherwise = (fromEnum x) - 38

calcTriplePrio :: [[Char]] -> Int
calcTriplePrio list = listSum (map (\x -> (prioValue (tripleIntersection x))) (makeListTriples list))

calcPrio :: [[Char]] -> Int
calcPrio list = listSum (map (\x -> (prioValue (singleIntersection (seperateListLeft 0 x, seperateListRight 0 x)))) list)



