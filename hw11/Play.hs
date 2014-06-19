module Play where

import Control.Applicative

pair :: Applicative f => f a -> f b -> f (a,b)
pair = liftA2 (,)

(*>) :: Applicative f => f a -> f b -> f b
(*>) = liftA2 (const id)
-- const normally does a -> (b -> a)
-- id does c -> c
-- so const id does (c -> c ) -> (b -> c -> c)
-- or a -> b -> b ; ; so returns second item always

sequenceA  :: Applicative f => [f a] -> f [a]
sequenceA [] = pure [] 
sequenceA (f:fs) = liftA2 (:) f (sequenceA fs) 

mapA :: Applicative f => (a -> f b) -> ([a] -> f [b])
mapA f =  sequenceA . map f


replicateA :: Applicative f => Int -> f a -> f [a]
replicateA n x = sequenceA (replicate n x)
