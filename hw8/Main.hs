module Main where
import Party

main :: IO ()
main = do
    raw <- readFile "company.txt"
    let guestList = maxFun $ read raw
    putStrLn $ (listHeader guestList) ++ "\n" ++ (guestNames guestList)
