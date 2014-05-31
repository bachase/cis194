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

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) Empty x = x
(+++) x Empty = x
(+++) x y = Append (mappend (tag x) (tag y) ) x y

instance (Monoid b) => Monoid (JoinList b a) where
    mempty = Empty
    mappend = (+++)

foldJ :: (Sized b, Monoid b, Monoid r) =>
    r -> (Int -> JoinList b a -> r) -> Int -> JoinList b a -> r
foldJ e _ _ Empty = e
foldJ e f n (Append _ l1 l2) = (foldJ e f n l1) <> (foldJ e f (n-n1) l2)
    where n1 = getSize . size $ tag l1
foldJ e f n l = f n l

indexJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> Maybe a
-- finds the JoinList element at specified index
indexJ _ Empty = Nothing
indexJ i (Single n x) 
    | i == 0    = Just x
    | otherwise = Nothing
indexJ i (Append _ l1 l2)
    | i < n1 = indexJ i l1
    | otherwise = indexJ (i - n1) l2
    where
      n1 = getSize . size $ tag l1

dropJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
dropJ = = Empty (\n l-> if (n <= 0) then l else Empty) 

takeJ :: (Sized b, Monoid b) =>
         Int -> JoinList b a -> JoinList b a
takeJ = Empty (\n l-> if (n > 0) then l else Empty) 

jlToList :: JoinList m a -> [a]
jlToList Empty           = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2
