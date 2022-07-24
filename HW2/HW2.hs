-- CptS 355 - Fall 2019 Assignment 2
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework

{-
Code by: Ian McLerran
Discussed with: No-one
-}

module HW2
     where

{- 1-  merge2 & merge2Tail & mergeN - 22% -}
--merge2
--merge2 l1 (x:xs) = 
merge2 :: (Ord a) => [a] -> [a] -> [a]
merge2 []  l2 = l2
merge2 (x:xs) l2 = instL x (merge2 xs l2)
     where
          instL n [] = [n]
          instL n (x:xs)  
               | n > x = x : instL n xs
               | otherwise = n : x : xs

--merge2Tail
merge2Tail :: (Ord a) => [a] -> [a] -> [a]
merge2Tail l1 [] = l1
merge2Tail l1 (x:xs) = merge2Tail (instL x l1) xs
     where
          instL n [] = [n]
          instL n (x:xs)  
               | n > x = x : instL n xs
               | otherwise = n : x : xs

--mergeN
mergeN :: (Foldable t, Ord a) => t [a] -> [a]
mergeN ll = foldr merge2 [] ll


{- 2 - getInRange & countInRange - 18% -}

--getInRange
getInRange :: (Ord a) => a -> a -> [a] -> [a]
getInRange lb ub xs = filter (inRange lb ub) xs
          where
               inRange lb ub x = x > lb && x < ub

--countInRange
countInRange :: Ord a => a -> a -> [[a]] -> Int
countInRange lb ub xss = sum (map (length . getInRange lb ub) xss)


{- 3 -  addLengths & addAllLengths - 18% -}

data LengthUnit =  INCH  Int | FOOT  Int | YARD  Int
                   deriving (Show, Read, Eq)

-- addLengths 
addLengths :: LengthUnit -> LengthUnit -> LengthUnit
addLengths (INCH l1) (INCH l2) = INCH (l1 + l2)
addLengths (FOOT l1) (FOOT l2) = INCH (l1*12 + l2*12)
addLengths (YARD l1) (YARD l2) = INCH (l1*36 + l2*36)
addLengths (INCH l1) (FOOT l2) = INCH (l1 + l2*12)
addLengths (FOOT l1) (INCH l2) = INCH (l1*12 + l2)
addLengths (FOOT l1) (YARD l2) = INCH (l1*12 + l2*36)
addLengths (YARD l1) (FOOT l2) = INCH (l1*36 + l2*12)
addLengths (INCH l1) (YARD l2) = INCH (l1 + l2*36)
addLengths (YARD l1) (INCH l2) = INCH (l1*36 +l2)

-- addAllLengths
addAllLengths :: Foldable t => [t LengthUnit] -> LengthUnit
addAllLengths xss = foldr (addLengths) (INCH 0) (map (foldr (addLengths) (INCH 0)) xss)
-- A more consice solution (without map): 
-- addAllLengths xss = foldr (addLengths . foldr (addLengths) (INCH 0)) (INCH 0) xss


{-4 - sumTree and createSumTree - 22%-}

data Tree a = LEAF a | NODE a  (Tree a)  (Tree a) 
              deriving (Show, Read, Eq)
 
--sumTree
sumTree :: Num p => Tree p -> p
sumTree (LEAF v) = v
sumTree (NODE v  lhs  rhs) = sumTree lhs + sumTree rhs


--createSumTree
createSumTree :: Num a => Tree a -> Tree a
createSumTree (LEAF v) = LEAF v
createSumTree (NODE v  lhs  rhs) = NODE (sumTree lhs + sumTree rhs)  (createSumTree lhs)  (createSumTree rhs)


{-5 - foldListTree - 20%-}
data ListTree a = ListLEAF [a] | ListNODE [ListTree a]
                  deriving (Show, Read, Eq)

foldListTree :: (a -> a -> a) -> a -> ListTree a -> a
foldListTree op b (ListLEAF lst) = foldl op b lst
foldListTree op b (ListNODE []) = b
foldListTree op b (ListNODE (t:ts)) = foldListTree op (foldListTree op b t) (ListNODE ts)


{- 6- Create two tree values :  Tree Integer  and  listTree a ;  Both trees should have at least 3 levels. -}

-- -- Tree Integer:
-- leaf nodes:
l11 = LEAF 11
l12 = LEAF 22
l13 = LEAF 33
l14 = LEAF 44
l15 = LEAF 55
-- interior nodes:
n11 = NODE 1 l11 l13
n12 = NODE 2 l12 l14
n13 = NODE 3 n11 n12
-- root node:
t11 = NODE 4 n13 l15


-- -- ListTree a:
-- leaf nodes:
l21 = ListLEAF ["learning"," "]
l22 = ListLEAF ["haskell"," "]
l23 = ListLEAF ["is"," "]
l24 = ListLEAF ["loads"," "]
l25 = ListLEAF ["of"," "]
l26 = ListLEAF ["fun"," "]
l27 = ListLEAF ["for"," "]
l28 = ListLEAF ["everyone","!"]
-- interior nodes:
n21 = ListNODE [l21, l22, l23]
n22 = ListNODE [l24]
n23 = ListNODE []
n24 = ListNODE [n21, n22]
n25 = ListNODE [n23, l25, l26]
n26 = ListNODE [n24, n25]
-- root node:
t21 = ListNODE [n26, l27, l28]


-- -- ListTree a:
-- leaf nodes:
l31 = ListLEAF [[2, 32], [16], []]
l32 = ListLEAF []
l33 = ListLEAF [[128, 64, 4, 1024], [4096], [8192, 256]]
l34 = ListLEAF [[512, 8], [2048, 1]]
-- interior nodes:
n31 = ListNODE [l34, l31]
n32 = ListNODE [l32, l33]
-- root node:
t31 = ListNODE [n32, n31]