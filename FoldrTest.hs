module FoldrTest 
    where

import Data.Char (toUpper)
import Prelude (foldr, Num)

append :: (Num a) => [a] -> a -> [a]
append xs e = foldr (:) [e] xs

-- append [] e = [e]
-- append (x:xs) e = x : append xs e