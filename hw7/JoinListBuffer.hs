{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinListBuffer where

import JoinList
import Scrabble
import Sized
import Buffer

instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList
  fromString   = foldl (\r c -> r +++ (strToLine c)) Empty . lines
  line n       = indexJ n
  replaceLine n l = uncurry replaceLine' . splitAtJ n
      where replaceLine' pre Empty = pre
            replaceLine' pre post = pre +++ (strToLine l) +++ (dropJ 1 post)
  numLines     = getSize . size . tag
  value        = getScore . fst . tag
