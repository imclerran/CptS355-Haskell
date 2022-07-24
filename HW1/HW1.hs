-- CptS 355 - Fall 2019 Assignment 1
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework
-- Written by: Ian McLerran
-- Discussed with: no-one.

module HW1
     where

import Data.Char (toUpper)

-- 1. exists
{-
   in the signature for exists, 'Eq t =>' indicates that 't' must be restricted to types which can be compared for equality.
 -}
exists :: Eq t => t -> [t] -> Bool
exists e list = e `elem` list

-- 2. listUnion
listUnion :: (Eq a, Ord a) => [a] -> [a] -> [a]
listUnion l1 l2 =
     let stripdups [] = []
         stripdups (x:xs) =
          if x `exists` xs
               then stripdups xs
               else x : stripdups xs
     in stripdups (l1 ++ l2)

-- 3. replace
replace :: (Eq t1, Num t1) => t1 -> t2 -> [t2] -> [t2]
replace n v [] = []
replace 0 v (h:t) = v : t
replace n v (h:t) = h : replace (n-1) v t


-- 4. prereqFor
{-
    The following (commented out) code for prereqFor, is fully working code, with one exception.
    Because of the type signature, (Eq t), I could not use String/Char operations, meaning
    I could not use the code needed to compare Strings ignoring case (since the type might not be String).
        EG: "MATH216", vs "Math216"
    Thus to meet the required type signature, the code will fail if the case does not match in the course name.
    To remmedy this, I have written a second prereqFor function, wich has a type signature of:
        [(String, [String])] -> String -> [String]
    This function with ammended type signature will work for all cases.
-}
-- prereqFor :: (Eq t) => [(t, [t])] -> t -> [t]
-- prereqFor [] crs = []
-- prereqFor (c:cs) crs =
--           if exists crs (snd c)
--                then fst c : prereqFor cs crs
--                else prereqFor cs crs

prereqFor :: [(String, [String])] -> String -> [String]
prereqFor [] crs = []
prereqFor (c:cs) crs =
     let
          existsNC :: String -> [String] -> Bool
          existsNC e [] = False
          existsNC e (x:xs) = (strcmp e x || existsNC e xs)
          ----
          compareNoCase :: Char -> Char -> Bool
          compareNoCase c1 c2 = toUpper c1 == toUpper c2
          ----
          strcmp :: String -> String -> Bool
          strcmp [] [] = True
          strcmp (x:xs) [] = False
          strcmp [] (y:ys) = False
          strcmp (x:xs) (y:ys) = compareNoCase x y && strcmp xs ys

     in
          if existsNC crs (snd c)
               then fst c : prereqFor cs crs
               else prereqFor cs crs

-- 5. isPalindrome
isPalindrome :: String -> Bool
isPalindrome "" = False
isPalindrome str =
     let
          reverse :: String -> String
          reverse [] = []
          reverse (x:xs) = reverse xs ++ [x]
          ----
          {-
              Using reduceStr instead of stripchar will allow palindromes with special characters between the letters.
                  Eg: "I'm Mi" is an acceptable palindrome when using reduceStr.
                  Note: "R'A^C*E C`A,R!" is also an acceptable palindrome when using reduceStr.
          -}
          validCs = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ1234567890"
          ----
          reduceStr :: String -> String
          reduceStr [] = []
          reduceStr (x:xs) =
               if x `exists` validCs
                    then x : reduceStr xs
                    else reduceStr xs
          --
          stripchar :: Char -> String -> String
          stripchar c [] = []
          stripchar c (x:xs) =
               if x == c
                    then stripchar c xs
                    else x : stripchar c xs
          ----
          stripchars :: String -> String -> String
          stripchars cs [] = []
          stripchars cs (x:xs) =
               if x `exists` cs then stripchars cs xs else x : stripchars cs xs
          ----
          compareNoCase :: Char -> Char -> Bool
          compareNoCase c1 c2 = toUpper c1 == toUpper c2
          ----
          strcmp :: String -> String -> Bool
          strcmp [] [] = True
          strcmp (x:xs) [] = False
          strcmp [] (y:ys) = False
          strcmp (x:xs) (y:ys) = compareNoCase x y && strcmp xs ys
          ----
          lengthof :: String -> Integer
          lengthof [] = 0
          lengthof str = 1 + lengthof (tail str)
          ----
     in strcmp (stripchars " '" str) (reverse (stripchars " '" str)) && (lengthof str > 1)

-- 6. groupSumtoN
groupSumtoN :: (Ord a, Num a) => a -> [a] -> [[a]]
groupSumtoN n lst =
     let
          qsort :: (Ord a) => [a] -> [a]
          qsort [] = []
          qsort (x:xs) = qsort [y | y <- xs, y <= x] ++ [x] ++ qsort [y | y <- xs, y > x]
          ----
          sumL [] = 0
          sumL (x:xs) = x + sumL xs
          ----
          append :: (Num a) => [a] -> a -> [a]
          append [] e = [e]
          append (x:xs) e = x : append xs e
          ----
          gsnHelper :: (Ord a, Num a) => a -> [a] -> [a] -> [[a]]
          gsnHelper n [] grp = [grp]
          gsnHelper n (x:xs) [] = gsnHelper n xs [x]
          gsnHelper n (x:xs) grp =
               if sumL grp + x <= n
                    then gsnHelper n xs (append grp x)
                    else [grp] ++ gsnHelper n xs [x]

     in
          gsnHelper n (qsort lst) []


