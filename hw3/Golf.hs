module Golf where

import Data.List

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

-- Problem 3

numOccurencesOf :: Int -> [Int] -> Int
-- Count number occurences of of number in list
numOccurencesOf n = length . filter (== n)

strHistograms :: [Int] -> [String]
strHistograms xs = [ (replicate (maxHist - c) ' ') ++ (replicate c '*') | c <- counts]
    where counts = [ numOccurencesOf n xs | n <- [0..9] ]
          maxHist = maximum counts

histogram :: [Int] -> String
histogram xs = z ++ "\n==========\n0123456789\n"
    where z = intercalate "\n" . transpose $ strHistograms xs
