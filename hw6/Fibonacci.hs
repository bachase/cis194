{-# LANGUAGE FlexibleInstances #-}

import Data.List

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

fibs1 :: [Integer]
fibs1 = map fib [0..]

fibs2 :: [Integer]
fibs2 = unfoldr step (-1,1)
  where
     step (f0,f1) = Just (f2, (f1,f2))
        where f2 = f0 + f1


data Stream a = Cons a (Stream a)
   
streamToList :: Stream a -> [a]
streamToList (Cons v s) =  v:(streamToList s)

instance Show a => Show (Stream a) where
    show s = show . take 20 $ streamToList s

streamRepeat :: a -> Stream a
streamRepeat v = (Cons v (streamRepeat v))

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons v s) = Cons (f v) (streamMap f s)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f n = Cons n (streamFromSeed f (f n) )


nats :: Stream Integer
nats = streamFromSeed (+1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons v1 s1) s2 = (Cons v1 (interleaveStreams s2 s1))

rulerFunction :: Integer -> Stream Integer
rulerFunction n = interleaveStreams (streamRepeat n) (rulerFunction (n+1) )

ruler :: Stream Integer
ruler = rulerFunction 0
-- consider counting in binary representation
-- 0001, 0010, 0011, 0100, 0101, 0110, 0111
-- ruler function is how many times you right shift
-- to get a 1 in right-most bit.


-- Exercise 6

x :: Stream Integer
-- x = 0 + 1 * x + 0 * x^2 + 0 * x ^3 ...
x = (Cons 0 ( Cons 1 (streamRepeat 0)))

sHead :: Stream a -> a
sHead (Cons n _) = n

sTail :: Stream a -> Stream a
sTail (Cons _ s) = s

instance Num (Stream Integer) where
   fromInteger n = (Cons n (streamRepeat 0))
   negate = streamMap (*(-1))
   (+) s1 s2 = (Cons (h1 + h2) (t1 + t2))
       where
            h1 = sHead s1
            h2 = sHead s2
            t1 = sTail s1
            t2 = sTail s2

   (*) a b = (Cons (a0 * b0) (a0_bp + ap_b))
       where
            a0 = sHead a
            b0 = sHead b
            ap = sTail a
            bp = sTail b
            a0_bp = streamMap (* a0) bp
            ap_b = ap * b

instance Fractional (Stream Integer) where
    (/) a b = q
       where
            a0 = sHead a
            b0 = sHead b
            ap_b0 = streamMap (\v -> (div v b0)) (sTail a)
            bp_b0 = streamMap (\v -> (div v b0)) (sTail b)
            q = (Cons (div a0 b0) (ap_b0 - q * bp_b0) )
            
fibs3 :: Stream Integer
-- from noticing if Fibonnaci sequence are coefficients of power series
-- (Stream Integer) : F(X) = F_0 + F_1* x + F_2 * x^2 + ...
-- then x + x * F(x) + x^2 * F(x) = F(X) ; solve for F(X)
fibs3 = x/(1- x - x ^2 )


data Matrix a = Mat a a a a
    deriving Show

instance Num (Matrix Integer) where
    fromInteger n = Mat n 0 0 n
    (+) (Mat a1 b1 c1 d1) (Mat a2 b2 c2 d2) 
        = Mat (a1 + a2) (b1 + b2) (c1 + c2) (d1 + d2)
    (*) (Mat a1 b1 c1 d1) (Mat a2 b2 c2 d2) 
        = Mat (a1 * a2 + b1 * c2) 
                 (a1 * b1 + b1 * d2)
                 (c1 * a2 + d1 * c2)
                 (c1 * b1 + d1 * d2)
      
fib4 :: Integer -> Integer
fib4 0 = 0
fib4 n = getA ( (Mat 1 1 1 0) ^ n)
    where 
      getA (Mat a _ _ _ ) = a
