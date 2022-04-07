-- ex.1
myreplicate :: Int -> a -> [a]
myreplicate 0 _ = []
myreplicate n x = [x | i <- [1..n]]

-- ex.2
mytakewhile :: (a -> Bool) -> [a] -> [a]
mytakewhile f [] = []
mytakewhile f (x:xs) = if (f x) then [x] ++ (mytakewhile f xs) else [] ++ (mytakewhile f xs)

mycheck :: (a -> Bool) -> [a] -> Bool
mycheck f [] = True
mycheck f (x:xs) = if (f x) then mycheck f xs else False

-- ex.3
mydigitfinder :: [Char] -> [Char]
mydigitfinder [] = []
mydigitfinder (x:xs) = if ((x == '1') || (x == '2') || (x == '3') || (x == '4') || (x == '5') || (x == '6') || (x == '7') || (x == '8') || (x == '9') || (x == '0'))
                       then [x] ++ (mydigitfinder xs) 
                       else mydigitfinder xs

-- ex.4
applyToAll1 :: (a -> a) -> [a] -> [a]
applyToAll1 f [] = []
applyToAll1 f (x:xs) = [f x] ++ applyToAll1 f xs

applyToAll2 :: (a -> a) -> [a] -> [a]
applyToAll2 f xs = [f x | x <- xs]

-- ex.5
mypythotriads :: Int -> [(Int, Int, Int)]
mypythotriads n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x^2 + y^2 == z^2]

-- ex.6
mylength :: [Int] -> Int
mylength xs = foldr (\x len -> len + 1) 0 xs

myreverse :: [Int] -> [Int]
myreverse xs = foldr (\x l -> l ++ [x]) [] xs

mymap :: (a -> b) -> [a] -> [b]
mymap f xs = foldr (\x l -> [f x] ++ l) [] xs 

myfilter :: (a -> Bool) -> [a] -> [a]
myfilter f xs = foldr (\x l -> if f x then [x] ++ l else l) [] xs 

-- ex.7
mysum :: [Int] -> Int
mysum [] = 0
mysum (x:xs) = x + (mysum xs)

myfactorsgenerator :: Int -> [Int]
myfactorsgenerator n = [x | x <- [1..(n - 1)], n `mod` x == 0]

mycheckifperfect :: Int -> Bool
mycheckifperfect n = if mysum (myfactorsgenerator n) == n then True else False 

myperfectnumbergenerator :: Int -> [Int]
myperfectnumbergenerator n = [x | x <- [1..n], mycheckifperfect x]

-- main
main :: IO ()
main = print (myperfectnumbergenerator 1000); 