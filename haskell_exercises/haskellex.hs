-- ses 1
-- ex 1
mylast [x] = x
mylast (x:xs) = mylast xs

myinit [x] = []
myinit (x:xs) = [x] ++ (myinit xs)

-- ex 2
False || False = False
_ || _ = True

-- ex 7
mysize :: [a] -> Int
mysize [] = 0
mysize (x:xs) = 1 + (mysize xs)

mytake :: Int -> [a] -> [a]
mytake 1 (x:xs) = [x]
mytake n [] = []
mytake n (x:xs) = if ((mysize ([x] ++ xs)) == n) then [x] ++ xs else ([x] ++ (mytake (n-1) xs))

-- ses 2


