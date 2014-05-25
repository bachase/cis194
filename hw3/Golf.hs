module Golf where

takeEvery :: Int -> [a] -> [a]
takeEvery n = map snd . filter (\(x,y) -> (mod x n) == 0) . zip [1..]

skips :: [a] -> [[a]]
skips [] = []
skips xs = skips' xs (length xs)
   where
       skips' xs 1 = [xs]
       skips' xs n = skips' xs (n-1) ++ [(takeEvery n xs)]

--- 
localMaxima :: [Integer] -> [Integer]
localMaxima z = map (\(a,b,c) -> b) . filter (\(a,b,c) -> b > a && b > c ) $ zip3 z (drop 1 z) (drop 2 z)


histogram :: [Integer] -> String
