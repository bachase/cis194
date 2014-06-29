{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values

newtype DieValue = DV { unDV :: Int } 
  deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random           = first DV . randomR (1,6)
  randomR (low,hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

dice :: Int -> Rand StdGen [DieValue]
dice n = sequence (replicate n die)
------------------------------------------------------------
-- Risk

type Army = Int

data Battlefield = Battlefield { attackers :: Army, defenders :: Army }
    deriving Show

numDefenders :: Battlefield -> Army
numDefenders = min 2 . defenders

numAttackers :: Battlefield -> Army
numAttackers f = min 3 . max 0 $ ((attackers f) - 1)


tally :: Battlefield -> [DieValue] -> [DieValue] -> Battlefield
tally f as ds = Battlefield ( (attackers f) + cA ) ( (defenders f) + cD )
    where
        outcomes = zip (reverse $ sort as) (reverse $ sort ds)
        (cA, cD) = foldl helper (0,0) outcomes 
        helper (dA,dD) (aVal, dVal)
            | aVal > dVal = (dA, dD - 1)
            | otherwise   = (dA - 1, dD)



battle :: Battlefield -> Rand StdGen Battlefield
battle f = do
   attackRolls  <- dice $ numAttackers f
   defenseRolls <- dice $ numDefenders f
   return (tally f attackRolls defenseRolls)

invade :: Battlefield -> Rand StdGen Battlefield
invade f = do
    f' <- battle f
    if isDone f'
        then return f'
        else invade f'
    where
       isDone (Battlefield a d) 
           | d == 0    = True
           | a <= 1    = True
           | otherwise = False

successProb :: Battlefield -> Rand StdGen Double
successProb f = do
    fs <- sequence (replicate 1000 $ invade f)
    return ((sum $ map attackersWon fs)/1000)
 where
    attackersWon (Battlefield _ 0) = 1
    attackersWon _                 = 0
