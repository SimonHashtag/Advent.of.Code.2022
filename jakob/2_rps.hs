import ListActions

--exercise 1
run :: IO Int
run = do
  contents <- readFile "vals/values2.txt" 
  let xs = lines contents     
  return (listSum (map calcPoints (map stringToHands xs)))

--exercise 2
run2 :: IO Int
run2 = do
  contents <- readFile "vals/values2.txt" 
  let xs = lines contents     
  return (listSum (map calcPoints (map changeList (map stringToHands xs))))

data Hand = A | B | C | X | Y | Z
    deriving Show

stringToHands :: String -> (Hand,Hand)
stringToHands (u:' ':q:"") = (charHand u, charHand q)

charHand :: Char -> Hand
charHand x = case x of
    'A' -> A
    'B' -> B
    'C' -> C
    'X' -> X
    'Y' -> Y
    'Z' -> Z

handVal :: Hand -> Int
handVal X = 1
handVal Y = 2
handVal Z = 3

handWin :: Hand -> Hand -> Int
handWin A q = case q of
    X -> 3
    Y -> 6
    Z -> 0
handWin B q = case q of
    X -> 0
    Y -> 3
    Z -> 6 
handWin C q = case q of
    X -> 6
    Y -> 0
    Z -> 3


calcPoints :: (Hand, Hand) -> Int
calcPoints (a,b) = (handVal b) + (handWin a b)

changeList :: (Hand, Hand) -> (Hand, Hand)
changeList (A,q) = case q of
    X -> (A,Z)
    Y -> (A,X)
    Z -> (A,Y)
changeList (B,q) = case q of
    X -> (B,X)
    Y -> (B,Y)
    Z -> (B,Z)
changeList (C,q) = case q of
    X -> (C,Y)
    Y -> (C,Z)
    Z -> (C,X)
    
