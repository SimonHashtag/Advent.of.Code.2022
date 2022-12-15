import ListActions

--exercise 1
run :: IO Int
run = do
  contents <- readFile "vals/values6.txt" 
  return (bufferCounter 0 contents)

--exercise 2
run2 :: IO Int
run2 = do
  contents <- readFile "vals/values6.txt" 
  return (messageCounter 0 contents)

bufferCounter :: Int -> [Char] -> Int
bufferCounter num (a:b:c:[]) = num + 3
bufferCounter num (a:b:c:d:xs) = if allDifferent [a,b,c,d] then num+4
                                 else bufferCounter (num+1) (b:c:d:xs)

messageCounter :: Int -> [Char] -> Int
messageCounter num (a:b:c:d:e:f:g:h:i:j:k:l:m:[]) = num + 13
messageCounter num (a:b:c:d:e:f:g:h:i:j:k:l:m:n:xs) = if allDifferent [a,b,c,d,e,f,g,h,i,j,k,l,m,n] then num+14
                                 else messageCounter (num+1) (b:c:d:e:f:g:h:i:j:k:l:m:n:xs)


