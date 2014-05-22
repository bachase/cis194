-- HW 1 solutions for cis198 course
-- http://www.seas.upenn.edu/~cis194/hw/01-intro.pdf

toDigits :: Integer -> [Integer]
toDigits = toDigits' []
    where
       toDigits' acc n
          | n <= 0      = acc
          | otherwise   = toDigits' ((mod n 10):acc) (div n 10)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . f . reverse
    where
        f (x:y:zs) = x:(2*y):(f zs)
        f a        = a
        
sumDigits :: [Integer] -> Integer
sumDigits = foldl (\acc x -> acc + (sum $ toDigits x)) 0

validate :: Integer -> Bool
validate a = (mod det 10) == 0
    where det = sumDigits . doubleEveryOther $ toDigits a

-- Normal function f g h x -> (((f g) h) x)
-- Want (sumDigits ( doubleEveryOther (toDigits a)))
-- sumDigits . doubleEveryOther . toDigits a is close
-- but toDigits a has higher precedence


type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c = first ++ second ++ third
    where first  = hanoi (n-1) a c b
          second = [(a,b)]
          third  = hanoi (n-1) c b a
