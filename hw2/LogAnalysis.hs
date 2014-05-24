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

messageTime :: LogMessage -> Int
messageTime (LogMessage _ t _) = t
messageTime (Unknown _)        = 0

insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) inTree = inTree
insert inMesg Leaf        = Node Leaf inMesg Leaf
insert inMesg (Node treeL m treeR)
    | (messageTime inMesg < messageTime m) = Node (insert inMesg treeL) m treeR
    | otherwise                            = Node treeL m (insert inMesg treeR)

build :: [LogMessage] -> MessageTree
build = foldr insert Leaf

inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node treeL m treeR) = (inOrder treeL) ++ [m] ++ (inOrder treeR)

isBadErrMsg :: Int -> LogMessage -> Bool
isBadErrMsg d (LogMessage (Error severity) _ _) = severity > d
isBadErrMsg _ _= False

getMsg :: LogMessage -> String
getMsg (LogMessage _ _ s) = s
getMsg (Unknown s) = s

whatWentWrong :: [LogMessage] -> [String]
whatWentWrong = map getMsg .filter (isBadErrMsg 50) . inOrder . build
