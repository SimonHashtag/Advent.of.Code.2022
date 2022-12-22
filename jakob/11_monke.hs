import ListActions

run = do
  contents <- readFile "vals/values11.txt" 
 -- let ys = translate (lines contents) []
  let ys = lines contents
  --return ((splitOn "" ys))
  return (map makeMonkey (splitOn "" ys))

data Monkey = Monkey [Int] Operation Int Int Int

data Operation = Plus Int | Times Int | Square

instance Show Operation where
  show (Plus x) = "+ " ++ show x
  show (Times x) = "* " ++ show x
  show Square = "* old"

opToOp :: Operation -> (Int -> Int)
opToOp (Plus n) = (+n)
opToOp (Times n) = (*n)
opTooOp (Square) = (^2)

instance Show Monkey where
  show (Monkey a op b c d) = "\nMonkey carrying Items: \n" ++  show a ++ "\nOperation: new = old "++ show op++"\nTest if divisible by " ++ show b ++ "\n   if True then throw to Monkey " ++ show c
    ++ "\n   if False then throw to Monkey " ++ show d ++ "\n"

makeMonkey :: [String] -> Monkey
makeMonkey [_,stIt, op, divBy, tr, fls] =
    Monkey (head (extractInts [stIt])) (getOp op) (head (head (extractInts [divBy]))) (head (head (extractInts [tr]))) (head (head (extractInts [fls])))
       where getOp x = if ('+' `elem` x) then Plus (head (head (extractInts [x])))
                       else if x == "  Operation: new = old * old" then Square
                       else Times (head (head (extractInts [x])))

changeWorry :: Int -> Operation -> Integer
changeWorry num op = round ((((opToOp op) num) / 3))