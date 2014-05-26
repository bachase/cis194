import Data.List

-- Exercise 1 
fun1 :: [Integer] -> Integer
fun1 []     = 1
fun1 (x:xs)
    | even x    = (x - 2)*fun1 xs
    | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' = product . map (\x -> x - 2) . filter even

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n 
    | even n    = n + fun2 (n `div` 2)
    | otherwise = fun2 (3*n + 1)

fun2' :: Integer -> Integer
fun2' = sum . filter even . takeWhile (/=1) .iterate (\n -> if even n then n `div` 2 else 3 * n + 1)

-- Exercise 2
data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

heightTree :: Tree a -> Integer
heightTree Leaf = -1
heightTree (Node h _ _ _) = h

insertTree :: a -> Tree a -> Tree a
-- insert a into tree keeping it balanced
insertTree x Leaf = Node 0 Leaf x Leaf
insertTree x (Node h left v right)
  | hL > hR   = Node h left v (insertTree x right)
  | hL < hR   = Node h (insertTree x left) v right
  | otherwise = Node (hL'+1) (insertTree x left) v right
  where hR    = heightTree right
        hL    = heightTree left
        left' = insertTree x left
        hL'   = heightTree left' 
    
foldTree :: [a] -> Tree a
foldTree = foldr insertTree Leaf

-- Exercise 3

xor :: [Bool] -> Bool
xor = odd . length . filter id

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x y -> (f x):y) []

-- Exercise 4
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map (\x -> 2*x + 1) $ [1..n] \\ bads
    where bads = [ i + j + 2*i*j | i <- [1..n], j <- [1..n]]


