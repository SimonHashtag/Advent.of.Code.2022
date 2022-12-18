import ListActions
import Data.Char as Char
--exercise 1
run :: IO (([(Int, Int)], [(Int, Int)]), Int)
run = do
  contents <- readFile "vals/values9.txt" 
  let xs = lines contents
  let ys = map translate xs
  let zs = movementSeriesLong ys ([(0,0),(0,0)],[])
  let as = allElems (last2Elem zs) []
  return (zs, length as)

--exercise 2
run2 :: IO (([(Int, Int)], [(Int, Int)]), Int)
run2 = do
  contents <- readFile "vals/values9.txt" 
  let a = lines contents
  let b = map translate a
  let c = movementSeriesLong b ([(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0),(0,0)],[])
  let d = allElems (last2Elem c) []
  return (c, length d)

data Movement = L| R| U| D
    deriving Show

last2Elem (_,x) = x

translate :: String -> (Movement, Int)
translate (c:' ':xs) = case c of
    'U' -> (U, (read xs::Int))
    'D' -> (D, (read xs::Int))
    'R' -> (R, (read xs::Int))
    'L' -> (L, (read xs::Int))

move ::((Int,Int)) -> Movement -> ((Int,Int))
move (x,y) m = case m of
    L -> (x - 1, y)
    R -> (x + 1, y)
    U -> (x,y + 1)
    D -> (x,y - 1)

follow :: ((Int,Int),(Int,Int)) -> (Int,Int)
follow ((a,b),(x,y)) = if a>(x+1) && b == y then ((a-1,b)) 
                       else if b>(y+1) && a == x then ((a,b-1))
                       else if x>(a+1) && b == y then ((a+1,b))
                       else if y>(b+1) && a == x then ((a,b+1))
                       else if a>x && b>(y+1) then (x+1, y+1)
                       else if a>x && b < (y-1) then (x+1, y-1)
                       else if a>(x+1) && b>y then (x+1, y+1)
                       else if a>(x+1) && b<y then (x+1, y-1)
                       else if a<x && b>(y+1) then (x-1, y+1)
                       else if a<x && b < (y-1) then (x-1, y-1)
                       else if a<(x-1) && b>y then (x-1, y+1)
                       else if a<(x-1) && b<y then (x-1, y-1)
                       else (x,y)

moveN :: Movement -> Int -> ((Int,Int),(Int,Int),[(Int,Int)]) -> ((Int,Int),(Int,Int),[(Int,Int)])
moveN m 0 a = a
moveN m n (h,t,xs) = let mvd = move h m
                         ys = follow (mvd,t)
                       in moveN m (n-1) (mvd,ys,(ys:xs))

followLong :: [(Int,Int)] -> [(Int,Int)]
followLong [] = []
followLong (a:x:xs) = let z = follow (a,x)
                      in a:(followLong (z:xs))
followLong a = a

moveNLong :: Movement -> Int -> ([(Int,Int)],[(Int,Int)]) -> ([(Int,Int)],[(Int,Int)])
moveNLong m 0 a = a
moveNLong m n ((x:xs),pos) = let mvd = move x m
                                 ys = followLong (mvd:xs)
                             in moveNLong m (n-1) (ys,(last ys):pos)

movementSeriesLong :: [(Movement,Int)] -> ([(Int,Int)],[(Int,Int)]) -> ([(Int,Int)],[(Int,Int)])
movementSeriesLong [] p = p
movementSeriesLong ((m,n):xs) a = movementSeriesLong xs (moveNLong m n a)




