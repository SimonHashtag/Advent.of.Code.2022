import ListActions
import Data.Char as Char
--exercise 1
run :: IO ([Tree],Int)
run = do
  contents <- readFile "vals/values8.txt" 
  let xs = lines contents
  let trs = makeTrees xs
  let zs = apply (map (vsbl trs) trs) trs
  return (zs, length zs)

--exercise 2
run2 :: IO (Int)
run2 = do
  contents <- readFile "vals/values8.txt" 
  let xs = lines contents
  let trs = makeTrees xs
  return (maximum (map (lookingDist trs) trs))

data Tree = Tree Int Int Int

instance Show Tree where
    show (Tree r c n) = if not (c == 0) then " (" ++ show r ++ "," ++ show c ++ "):" ++ show n
                        else "\n (" ++ show r ++ "," ++ show c ++ "):" ++ show n

makeTreesSingle :: Int -> String -> [Tree]
makeTreesSingle row str = func row 0 str
    where func _ _ "" = []
          func row col (x:xs) = [Tree row col (Char.digitToInt x)] ++ func row (col+1) xs

makeTrees :: [String] -> [Tree]
makeTrees str = func str 0
    where func [] _= []
          func (x:xs) n= (makeTreesSingle n x) ++ func xs (n+1)

getTree :: Int -> Int -> [Tree] -> Tree
getTree x y [] = Tree (-1) (-1) (-1)
getTree x y ((Tree a b c):xs) = if (x == a) && (y == b) then Tree a b c
                                else getTree x y xs

nbr :: Tree -> Int
nbr (Tree _ _ n) = n

data Direction = Abv | Blw | Lft | Rgt 

dirOf :: Direction -> Tree -> [Tree] -> [Tree]
dirOf _ _ [] = []
dirOf Abv (Tree a b c) ((Tree x y n):xs) = if b == y && x<a then (Tree x y n):(dirOf Abv (Tree a b c) xs)
                                       else (dirOf Abv (Tree a b c) xs)
dirOf Blw (Tree a b c) ((Tree x y n):xs) = if b == y && x>a then (Tree x y n):(dirOf Blw (Tree a b c) xs)
                                       else (dirOf Blw (Tree a b c) xs)
dirOf Lft (Tree a b c) ((Tree x y n):xs) = if b > y && x==a then (Tree x y n):(dirOf Lft (Tree a b c) xs)
                                       else (dirOf Lft (Tree a b c) xs)
dirOf Rgt (Tree a b c) ((Tree x y n):xs) = if b < y && x==a then (Tree x y n):(dirOf Rgt (Tree a b c) xs)
                                       else (dirOf Rgt (Tree a b c) xs)


vsbl :: [Tree] -> Tree -> Bool
vsbl trs tr = let height = nbr tr
                  leftMax = maxi (map nbr (dirOf Lft tr trs))
                  rightMax = maxi (map nbr (dirOf Rgt tr trs))
                  aboveMax = maxi (map nbr (dirOf Abv tr trs))
                  belowMax = maxi (map nbr (dirOf Blw tr trs))
              in 
                (height > minimum [leftMax,rightMax,aboveMax,belowMax]) 

maxi :: [Int] -> Int
maxi [] = -1
maxi xs = maximum xs

apply :: [Bool] -> [Tree] -> [Tree]
apply [] _ = []
apply (True:xs) (y:ys) = y:(apply xs ys)
apply (False:xs) (_:ys) = apply xs ys

lookingDist :: [Tree] -> Tree -> Int
lookingDist trs tr = let height = nbr tr
                         viewDirAbv = cutOff height (reverse (map nbr (dirOf Abv tr trs)))
                         viewDirBlw = cutOff height (map nbr (dirOf Blw tr trs))
                         viewDirLft = cutOff height (reverse (map nbr (dirOf Lft tr trs)))
                         viewDirRgt = cutOff height (map nbr (dirOf Rgt tr trs))
                     in 
                        viewDirAbv*viewDirBlw*viewDirLft*viewDirRgt
cutOff :: Int -> [Int] -> Int
cutOff _ [] = 0
cutOff max (x:xs) = if max > x then 1 + (cutOff max xs)
                    else 1
