module Party where

import Employee
import Data.Monoid
import Data.Tree
import Data.List

glCons :: Employee -> GuestList -> GuestList
glCons e (GL gl f) = GL (e:gl) (f + empFun e)

instance Monoid GuestList where
  mempty = GL [] 0
  mappend (GL l1 f1) (GL l2 f2) = GL (l1 ++ l2) (f1 + f2)

moreFun :: GuestList -> GuestList -> GuestList
moreFun a@(GL _ f1) b@(GL _ f2)
   |  f1 > f2   = a
   |  otherwise = b

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f (Node a xs) = f a (map (treeFold f) xs)

nextLevel :: Employee -> [(GuestList, GuestList)]
             -> (GuestList, GuestList)
nextLevel e gs = (withE,withoutE)
   where
      inclSubBoss = mconcat $ map fst gs
      exclSubBoss = mconcat $ map snd gs
      withE = glCons e exclSubBoss
      withoutE = inclSubBoss

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel


guestNames :: GuestList -> String
guestNames (GL es _) = unlines . sort $ map empName es

listHeader :: GuestList -> String
listHeader (GL _ f) = "Total Fun: " ++ (show f)

