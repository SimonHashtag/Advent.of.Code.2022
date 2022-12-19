import ListActions
run = do
  contents <- readFile "vals/values10.txt" 
  let ys = translate (lines contents) []
  return (listSum (map (\n-> n*(getNth ys (n-1))) [20,60,100,140,180,220]))

run2 = do
  contents <- readFile "vals/values10.txt" 
  putStrLn (crt 1 (translate (lines contents) []))

translate [] _= []
translate (("noop"):xs) [] = [1] ++ translate xs [1]
translate (("noop"):xs) ys = let newYs = [last ys] 
                             in newYs ++ (translate xs (ys ++ newYs))
translate (('a':'d':'d':'x':' ':ys):xs) []= let newYs = [1,1,1+read ys::Int]
                                            in newYs ++ (translate xs newYs)
translate (('a':'d':'d':'x':' ':ys):xs) zs= let a = last zs
                                                b = read ys::Int
                                                newYs = [a,a+b]
                                            in newYs ++ (translate xs newYs)
crt _ []  = []
crt 41 xs = "\n" ++ crt 1 xs
crt n (x:xs) = (currChar n x):crt (n+1) xs
    where currChar n sprite | (abs (sprite-n))<=1 = '■'
                            | True =' '
