-- ex.1
mylast :: [a] -> a
mylast [x] = x
mylast (x:xs) = mylast xs

myinit :: [a] -> [a]
myinit [x] = []
myinit (x:xs) = [x] ++ (myinit xs)

-- ex.2
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

(|||) :: Bool -> Bool -> Bool
True ||| _ = True
_ ||| b = b

-- ex.3
mytaila :: [a] -> [a]
mytaila x = if (null x) then [] else tail x

mytailb :: [a] -> [a]
mytailb x | null x = []
          | otherwise = tail x

mytailc :: [a] -> [a]
mytailc [] = []
mytailc x = tail x

-- ex.4
mymaxa :: (Int, Int, Int) -> Int
mymaxa (a,b,c) = if a > b && a > c then a else
                  if b > a && b > c then b else c

mymaxb :: (Int, Int, Int) -> Int
mymaxb (a,b,c) | a > b && a > c = a
               | b > a && b > c = b
               | otherwise      = c

mymax2 :: Int -> Int -> Int
mymax2 a b = if a > b then a else b

mymaxc :: Int -> Int -> Int -> Int
mymaxc a b c = mymax2 a (mymax2 b c)

-- ex.5
mycontains :: Int -> [Int] -> Bool
mycontains y xs = [] /= [x | x <- xs, x == y]

-- ex.6
myinsert :: Int -> [Int] -> [Int]
myinsert x xs = (([y | y <- xs, (x >= y)]) ++ [x]) ++ ([y | y <- xs, (x < y)])

myinsertionsort :: [Int] -> [Int]
myinsertionsort [] = []
myinsertionsort (x:xs) = myinsert x (myinsertionsort xs)

-- ex.7
mysize :: [a] -> Int
mysize [] = 0
mysize (x:xs) = 1 + (mysize xs)

mytake :: Int -> [a] -> [a]
mytake 1 (x:xs) = [x]
mytake n [] = []
mytake n (x:xs) = if ((mysize ([x] ++ xs)) == n) then [x] ++ xs else ([x] ++ (mytake (n-1) xs))

-- main
main :: IO ()
main = print (last [1,2,3]);