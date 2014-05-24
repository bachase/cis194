{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

toInt :: String -> Int
toInt = read

parseMessage :: String -> LogMessage
-- Do we need to handle ill-formed? e.g E 20 this is missing timestamp
parseMessage s = f $ words s
    where 
       f ("I":t:zs)   = LogMessage Info (toInt t) (unwords zs)
       f ("W":t:zs)   = LogMessage Warning (toInt t) (unwords zs)
       f ("E":e:t:zs) = LogMessage (Error (toInt e)) (toInt t) (unwords zs)
       f _            = Unknown s

parse :: String -> [LogMessage]
parse = map parseMessage . lines
