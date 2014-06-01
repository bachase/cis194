module Main where

import JoinListBuffer
import JoinList
import Scrabble
import Sized
import Editor

def :: JoinList (Score, Size) String
def = strToLine "This is an empty buffer"

main = runEditor editor def
