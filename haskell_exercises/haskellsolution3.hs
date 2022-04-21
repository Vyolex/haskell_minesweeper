-- ex.1
data Colour = Green | AlmostGreen | Blue
instance Eq Colour where
    Blue == Blue = True
    Blue == _ = False
    _ == Blue = False
    _ == _ = True

-- ex.2
data Nat = Zero | Succ Nat
int2nat :: Int -> Nat
int2nat i = if i == 0 then Zero
            else Succ (int2nat (i-1))

nat2int :: Nat -> Int
nat2int Zero = 0
nat2int (Succ n) = 1 + (nat2int n)

natadd :: Nat -> Nat -> Nat
natadd Zero n = n
natadd n Zero = n
natadd m (Succ n) = natadd (Succ m) n 

natmult :: Nat -> Nat -> Nat
natmult Zero _ = Zero
natmult _ Zero = Zero
natmult m (Succ Zero) = m
natmult (Succ Zero) m = m
natmult n (Succ m) = natadd n (natmult n m)  
-- > implement n * m as n + (n * (m - 1))

instance Show Nat where
    show Zero = "0"
    show (Succ n) = "Succ (" ++ ((show n) ++ ")") 

instance Num Nat where
    a + b = natadd a b
    a * b = natmult a b
    
-- ex.3
data Tree a = Node a (Tree a) (Tree a) | Leaf a

occurs :: Eq a => Tree a -> a -> Bool
occurs (Leaf a) b = a == b
occurs (Node a t1 t2) b = if (a == b) then True else (occurs t1 b) || (occurs t2 b)

nateq :: Nat -> Nat -> Bool
nateq Zero Zero = True
nateq Zero _ = False
nateq _ Zero = False
nateq (Succ n) (Succ m) = nateq n m

instance Eq Nat where
    a == b = nateq a b

-- ex.4
checkPalindrome :: [Char] -> Bool
checkPalindrome [] = True
checkPalindrome [x] = True
checkPalindrome (x:xs) = if x == (last xs) 
                         then (True && checkPalindrome (init xs))
                         else False

isPalindrome :: IO ()
isPalindrome = do
               putStrLn "What word would you like me to check?";
               let str = ""; 
               str <- getLine;
               if checkPalindrome str then putStrLn "Your word was indeed a palindrome! :)" 
               else putStrLn "That ain't no palindrome, you fool!";
-- main
main :: IO ()
main = isPalindrome; 