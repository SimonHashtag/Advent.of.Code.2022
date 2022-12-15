import ListActions

--exercise 1
run :: IO [Stack]
run = do
  contents <- readFile "vals/values5.txt" 
  let xs = lines contents     
  return (moveInstructions stacks (extractInts xs))

--exercise 2
run2 :: IO [Stack]
run2 = do
  contents <- readFile "vals/values5.txt" 
  let xs = lines contents     
  return (moveInstructions2 stacks (extractInts xs))

data Stack = Stack Int [Char]

instance Show Stack where
    show (Stack a b) = "[Stack " ++ show a ++ "] : " ++  b ++ "\n"


push :: Stack -> Char -> Stack
push (Stack a b) c = Stack a (c:b)

pop :: Stack -> Stack
pop (Stack a []) = Stack a []
pop (Stack a (b:bs)) = Stack a bs

top :: Stack -> Char
top (Stack _ (a:_)) = a

stacks :: [Stack]
stacks = [ (Stack 1 ['G','W','L','J','B','R','T','D']),
           (Stack 2 ['C','W','S']),
           (Stack 3 ['M','T','Z','R']),
           (Stack 4 ['V','P','S','H','C','T','D']),
           (Stack 5 ['Z','D','L','T','P','G']),
           (Stack 6 ['D','C','Q','J','Z','R','B','F']),
           (Stack 7 ['R','T','F','M','J','D','B','S']),
           (Stack 8 ['M','V','T','B','R','H','L']),
           (Stack 9 ['V','S','D','P','Q'])]

findStack :: Int -> [Stack] -> Stack
findStack x [] = Stack (-1) []
findStack x ((Stack y st):xs) = if y == x then Stack y st
                                         else findStack x xs

putOnStack :: [Stack] -> Char -> Int -> [Stack]
putOnStack [] _ _= [] 
putOnStack ((Stack p x):xs) char pos = if pos == p then ((push (Stack p x) char):xs)
                                                 else (Stack p x):(putOnStack (xs) char pos)
popFromStack :: [Stack] -> Int -> [Stack]
popFromStack [] _ = []
popFromStack ((Stack a x):xs) pos = if pos == a then ((pop (Stack a x)):xs)
                                    else ((Stack a x):popFromStack xs pos)

move :: [Stack] -> Int -> Int -> [Stack]
move sts x y = let st1 = popFromStack sts x
                    in putOnStack st1 (top (findStack x sts)) y

moveN :: [Stack] -> [Int] -> [Stack]
moveN sts (0:y:z:_) = sts
moveN sts (x:y:z:xs) = moveN (move sts y z) (x-1:y:z:xs)

moveNatOnce :: [Stack] -> [Int] -> [Stack]
moveNatOnce sts (x:y:z:xs) = let stsTemp = (Stack 0 []):sts
    in 
      tail (moveN (moveN stsTemp (x:y:0:xs)) (x:0:z:xs))

moveInstructions :: [Stack] -> [[Int]] -> [Stack]
moveInstructions st [] = st
moveInstructions st (x:xs) = moveInstructions (moveN st x) xs

moveInstructions2 :: [Stack] -> [[Int]] -> [Stack]
moveInstructions2 st [] = st
moveInstructions2 st (x:xs) = moveInstructions2 (moveNatOnce st x) xs