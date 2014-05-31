module JoinList where

import Data.Monoid
import Sized

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
    deriving (Eq, Show)

tag :: Monoid m => JoinList m a -> m
tag Empty = mempty
tag (Single m _ ) = m
tag (Append m _ _) = m

valJ :: JoinList m a -> Maybe a
-- Returns value stored in single list, otherwise nothing
valJ Empty = Nothing
valJ (Single _ x) = Just x
valJ _ = Nothing


(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) x y = Append (mappend (tag x) (tag y) ) x y

instance (Monoid b) => Monoid (JoinList b a) where
    mempty = Empty
    mappend = (+++)

foldJ :: (Sized b, Monoid b) =>
    r -> (Int -> JoinList b a -> r) -> (Int -> Int -> r -> r -> r)
    -> Int -> JoinList b a -> r
foldJ e _ _ _ Empty = e
foldJ e f g n (Append _ l1 l2) 
    = g n n1 (foldJ e f g n l1) (foldJ e f g (n-n1) l2)
        where n1 = getSize . size $ tag l1
foldJ _ f _ n l = f n l

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
-- finds the JoinList element at specified index
indexJ = foldJ Nothing f g
    where
        f n l = if(n == 0) then valJ l else Nothing
        g n n1 r1 r2 = if(n < n1) then r1 else r2

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ = foldJ Empty (\n l-> if (n <= 0) then l else Empty) (\_ _ x y -> x +++ y)

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ = foldJ Empty (\n l-> if (n > 0) then l else Empty) (\_ _ x y -> x +++ y)

jlToList :: JoinList m a -> [a]
jlToList Empty           = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
