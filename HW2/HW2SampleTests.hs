{-Haskell is available for Windows, Mac, and Linux. Here's the download page: http://www.haskell.org/platform/.
We will be using the HUnit unit testing package in CptS 355.  

Example of using the HUnit unit test framework.  See  http://hackage.haskell.org/package/HUnit for additional documentation.
To run the tests type "runTestTT tests" at the Haskell prompt.  -}

{-
Updated by: Ian McLerran
Discussed with: No-one
-}

module HW2SampleTests
    where

import Test.HUnit
import Data.Char
import HW2

{- Two useful functions in the HUnit package are assertEqual and assertBool.
The arguments to 'assertEqual' are:
      a descriptive string
      the expected value
      the value being tested
The arguments to 'assertBool' are:
      a descriptive string
      the boolean value being tested
-}

-- Problem 1a
p1a_test1 = TestCase (assertEqual "merge2 [2,5,6,8,9] [1,3,4,5,7,8,10]" [1,2,3,4,5,5,6,7,8,8,9,10]  (merge2 [2,5,6,8,9] [1,3,4,5,7,8,10]) )
p1a_test2 = TestCase (assertEqual "merge2 [1,2,3] [1,2,3]" [1,1,2,2,3,3] (merge2 [1,2,3] [1,2,3]) ) 
p1a_test3 = TestCase (assertEqual "merge2 [-6,-4,-2] [1,3,5]" [-6,-4,-2,1,3,5] (merge2 [-6,-4,-2] [1,3,5]) )
p1a_test4 = TestCase (assertEqual "merge2 [] []" [] (merge2 ([]::[Int]) ([]::[Int])) )

-- Problem 1b
p1b_test1 = TestCase (assertEqual "merge2Tail [2,5,6,8,9] [1,3,4,5,7,8,10]" [1,2,3,4,5,5,6,7,8,8,9,10]  (merge2Tail [2,5,6,8,9] [1,3,4,5,7,8,10]) )
p1b_test2 = TestCase (assertEqual "merge2Tail [1,2,3] [1,2,3]" [1,1,2,2,3,3] (merge2Tail [1,2,3] [1,2,3]) ) 
p1b_test3 = TestCase (assertEqual "merge2Tail [-6,-4,-2] [1,3,5]" [-6,-4,-2,1,3,5] (merge2Tail [-6,-4,-2] [1,3,5]) )
p1b_test4 = TestCase (assertEqual "merge2Tail [] []" [] (merge2Tail ([]::[Int]) ([]::[Int])) )

-- Problem 1c
p1c_test1 = TestCase (assertEqual "mergeN [[3,4],[-3,-2,-1],[1,2,5,8,9]]" [-3,-2,-1,1,2,3,4,5,8,9]  (mergeN [[3,4],[-3,-2,-1],[1,2,5,8,9]]) )
p1c_test2 = TestCase (assertEqual "mergeN [[],['x','y','z'],['a','b','c']]" ['a','b','c','x','y','z'] (mergeN [[],['x','y','z'],['a','b','c']]) )
p1c_test3 = TestCase (assertEqual "mergeN []::[[Int]]" [] (mergeN []::[[Int]]) )

-- Problem 2a
p2a_test1 = TestCase (assertEqual "getInRange (-5) 5 [10,5,0,1,2,-5,-10]" [0,1,2]  (getInRange (-5) 5 [10,5,0,1,2,-5,-10]) )
p2a_test2 = TestCase (assertEqual "getInRange (-1) 1 [-2,2,3,4,5]" [] (getInRange (-1) 1 [-2,2,3,4,5]) )
p2a_test3 = TestCase (assertEqual "getInRange (-1) 1 [-6,-4,-2,0,0,2,4,6]" [0,0] (getInRange (-1) 1 [-6,-4,-2,0,0,2,4,6]) )
p2a_test4 = TestCase (assertEqual "getInRange 1 2 [1,2]" [] (getInRange 1 2 [1,2]) )
p2a_test5 = TestCase (assertEqual "getInRange (maxBound::Int) (minBound::Int) [1,2,3]" [] (getInRange (maxBound::Int) (minBound::Int) [1,2,3]) )
p2a_test6 = TestCase (assertEqual "getInRange (minBound::Int) (maxBound::Int) []" [] (getInRange (minBound::Int) (maxBound::Int) []) )

-- Problem 2b
p2b_test1 = TestCase (assertEqual "countInRange 3 10 [[1,2,3,4],[5,6,7,8,9],[10,11]]" 6 (countInRange 3 10 [[1,2,3,4],[5,6,7,8,9],[10,11]]) )
p2b_test2 = TestCase (assertEqual "countInRange (-5) 5 [[-10,-5,-4],[0,4,5],[],[10]]" 3 (countInRange (-5) 5 [[-10,-5,-4],[0,4,5],[],[10]]) )
p2b_test3 = TestCase (assertEqual "countInRange (minBound::Int) (maxBound::Int) [[-1000,1000],[1,2,3,4,5],[3,4,5,6,7]]" 12 (countInRange (minBound::Int) (maxBound::Int) [[-1000,1000],[1,2,3,4,5],[3,4,5,6,7]]) )
p2b_test4 = TestCase (assertEqual "countInRange (minBound::Int) (maxBound::Int) []::[[Int]]" 0 (countInRange (minBound::Int) (maxBound::Int) ([]::[[Int]])) )

-- Problem 3a
p3a_test1 = TestCase (assertEqual "addLengths (FOOT 2) (INCH 5)" (INCH 29) (addLengths (FOOT 2) (INCH 5)) ) 
p3a_test2 = TestCase (assertEqual "addLengths (YARD 3) (INCH (-3))"  (INCH 105) (addLengths (YARD 3) (INCH (-3))) )
p3a_test3 = TestCase (assertEqual "addLengths (INCH 12) (FOOT (-1))" (INCH 0) (addLengths (INCH 12) (FOOT (-1))) )
p3a_test4 = TestCase (assertEqual "addLengths (INCH (-24)) (YARD 3)" (INCH 84) (addLengths (INCH (-24)) (YARD 3)) )
p3a_test5 = TestCase (assertEqual "addLengths (YARD 2) (FOOT (-7))" (INCH (-12)) (addLengths (YARD 2) (FOOT (-7))) )
p3a_test6 = TestCase (assertEqual "addLengths (FOOT (-2)) (YARD (-1))" (INCH (-60)) (addLengths (FOOT (-2)) (YARD (-1))) )
p3a_test7 = TestCase (assertEqual "addLengths (INCH 3) (INCH 5)" (INCH 8) (addLengths (INCH 3) (INCH 5)) )
p3a_test8 = TestCase (assertEqual "addLengths (FOOT 1) (FOOT 2)" (INCH 36) (addLengths (FOOT 1) (FOOT 2)) )
p3a_test9 = TestCase (assertEqual "addLengths (YARD 1) (YARD (-3))" (INCH (-72)) (addLengths (YARD 1) (YARD (-3))) )

-- Problem 3b
p3b_test1 = TestCase (assertEqual "addAllLengths [[YARD 2, FOOT 1], [YARD 1, FOOT 2, INCH 10],[YARD 3]]" (INCH 262) (addAllLengths [[YARD 2, FOOT 1], [YARD 1, FOOT 2, INCH 10],[YARD 3]]) )
p3b_test2 = TestCase (assertEqual "" (INCH 0) (addAllLengths [[YARD (-1), INCH 5, FOOT 2],[INCH 13, FOOT (-1)],[INCH 6]]) )
p3b_test3 = TestCase (assertEqual "" (INCH 0) (addAllLengths ([]::[[LengthUnit]])) )

-- Problem 4a
p4a_test1 = TestCase (assertEqual ("sumTree "++(show t1)) 32 (sumTree t1) ) 
p4a_test2 = TestCase (assertEqual ("sumTree "++(show t11)) 165 (sumTree t11) )
p4a_test3 = TestCase (assertEqual "sumTree (NODE 1 (LEAF 0) (LEAF 0))" 0 (sumTree (NODE 1 (LEAF 0) (LEAF 0))) )
p4a_test4 = TestCase (assertEqual "sumTree (LEAF 5)" 5 (sumTree (LEAF 5)) )
p4a_test5 = TestCase (assertEqual "sumTree (LEAF 0)" 0 (sumTree (LEAF 0)) )

-- Problem 4b
p4bt1_output = NODE 32 (NODE 15 (NODE 9 (LEAF 4) (LEAF 5)) (LEAF 6)) (NODE 17 (LEAF 8) (LEAF 9))
p4bt2_output = NODE 165 (NODE 110 (NODE 44 (LEAF 11) (LEAF 33)) (NODE 66 (LEAF 22) (LEAF 44))) (LEAF 55)
p4bt3_output = NODE 0 (LEAF 0) (LEAF 0)

p4b_test1 = TestCase (assertEqual ("createSumTree "++(show t1)) p4bt1_output (createSumTree t1) ) 
p4b_test2 = TestCase (assertEqual ("createSumTree "++(show t11)) p4bt2_output (createSumTree t11) )
p4b_test3 = TestCase (assertEqual "createSumTree (NODE 1 (LEAF 0) (LEAF 0))" p4bt3_output (createSumTree (NODE 1 (LEAF 0) (LEAF 0))) )
p4b_test4 = TestCase (assertEqual "createSumTree (LEAF 5)" (LEAF 5) (createSumTree (LEAF 5)) )

-- Problem 5
p5t2_output = "School-of-Electrical-Engineering-and-Computer-Science-WSU"
p5t3_output = "learning haskell is loads of fun for everyone!"
p5t4_output = "       !aaadeeeeefffghiikllllnnnnoooorrrsssuvy"
p5t5_output = [1,2,4,8,16,32,64,128,256,512,1024,2048,4096,8192]

p5_test1 = TestCase (assertEqual ("foldListTree (+) 0 "++(show t4)) 36 (foldListTree (+) 0 t4 ) ) 
p5_test2 = TestCase (assertEqual ("foldListTree (++) \"\" "++(show t5)) p5t2_output (foldListTree (++) "" t5) ) 
p5_test3 = TestCase (assertEqual ("foldListTree (++) \"\" "++(show t21)) p5t3_output (foldListTree (++) "" t21) )
p5_test4 = TestCase (assertEqual ("foldListTree (++) \"\" "++(show t21)) p5t4_output (foldListTree (merge2) "" t21) )
p5_test5 = TestCase (assertEqual ("foldListTree (merge2) [] "++(show t31)) p5t5_output (foldListTree (merge2Tail) [] t31) )


-- Sample Tree Integer examples given in the assignment prompt; make sure to provide your own tree examples for both tree data types
-- Your trees should have minimum 3 levels. 
t1 = NODE 1
         (NODE 2 (NODE 3 (LEAF 4) (LEAF 5)) (LEAF 6)) 
         (NODE 7 (LEAF 8) (LEAF 9))
t2 = NODE 0
          (NODE 0 (LEAF 4) (NODE 0 (LEAF 8) (LEAF 9)))
          (NODE 0 (NODE 0 (LEAF 10) (NODE 0 (LEAF 12) (LEAF 13))) (LEAF 7))

t3 = NODE 0 (NODE 0 (NODE 0 (LEAF 4) (LEAF 5)) (LEAF 6))
                (NODE 0 (LEAF 8) (LEAF 9))

t4 = ListNODE 
 [ ListNODE [ ListLEAF [1,2,3],ListLEAF [4,5],ListNODE([ListLEAF [6], ListLEAF []]) ],    
   ListNODE [], 
   ListLEAF [7,8], 
   ListNODE [ListLEAF [], ListLEAF []] ]

l1 = ListLEAF ["School","-","of","-","Electrical"]
l2 = ListLEAF ["-","Engineering","-"]
l3 = ListLEAF ["and","-","Computer","-"]
l4 = ListLEAF ["Science"]
l5 = ListLEAF ["-WSU"]
n1 = ListNODE [l1,l2]
n2 = ListNODE [n1,l3]
t5 = ListNODE [n2,l4,l5]

-- My Trees can be found in HW2.hs, under problem 6

tests = TestList [ TestLabel "Problem 1a - test1 " p1a_test1,
                   TestLabel "Problem 1a - test2 " p1a_test2,
                   TestLabel "Problem 1a - test3 " p1a_test3,
                   TestLabel "Problem 1a - test4 " p1a_test4,
                   TestLabel "Problem 1b - test1 " p1b_test1,
                   TestLabel "Problem 1b - test2 " p1b_test2,
                   TestLabel "Problem 1b - test3 " p1b_test3,
                   TestLabel "Problem 1b - test4 " p1b_test4,
                   TestLabel "Problem 1c - test1 " p1c_test1,
                   TestLabel "Problem 1c - test2 " p1c_test2,
                   TestLabel "Problem 1c - test3 " p1c_test3,
                   TestLabel "Problem 2a - test1 " p2a_test1,
                   TestLabel "Problem 2a - test2 " p2a_test2,
                   TestLabel "Problem 2a - test3 " p2a_test3,
                   TestLabel "Problem 2a - test4 " p2a_test4,
                   TestLabel "Problem 2a - test5 " p2a_test5,
                   TestLabel "Problem 2a - test6 " p2a_test6,
                   TestLabel "Problem 2b - test1 " p2b_test1,
                   TestLabel "Problem 2b - test2 " p2b_test2,
                   TestLabel "Problem 2b - test3 " p2b_test3,
                   TestLabel "Problem 2b - test4 " p2b_test4,
                   TestLabel "Problem 3a - test1 " p3a_test1,
                   TestLabel "Problem 3a - test2 " p3a_test2,
                   TestLabel "Problem 3a - test3 " p3a_test3,
                   TestLabel "Problem 3a - test4 " p3a_test4,
                   TestLabel "Problem 3a - test5 " p3a_test5,
                   TestLabel "Problem 3a - test6 " p3a_test6,
                   TestLabel "Problem 3a - test7 " p3a_test7,
                   TestLabel "Problem 3a - test8 " p3a_test8,
                   TestLabel "Problem 3a - test9 " p3a_test9,
                   TestLabel "Problem 3b - test1 " p3b_test1,
                   TestLabel "Problem 3b - test2 " p3b_test2,
                   TestLabel "Problem 3b - test3 " p3b_test3,
                   TestLabel "Problem 4a - test1 " p4a_test1,
                   TestLabel "Problem 4a - test2 " p4a_test2,
                   TestLabel "Problem 4a - test3 " p4a_test3,
                   TestLabel "Problem 4a - test4 " p4a_test4,
                   TestLabel "Problem 4a - test5 " p4a_test5,
                   TestLabel "Problem 4b - test1 " p4b_test1,
                   TestLabel "Problem 4b - test2 " p4b_test2,
                   TestLabel "Problem 4b - test3 " p4b_test3,
                   TestLabel "Problem 4b - test4 " p4b_test4,
                   TestLabel "Problem 5 - test1 " p5_test1,
                   TestLabel "Problem 5 - test2 " p5_test2,
                   TestLabel "Problem 5 - test3 " p5_test3,
                   TestLabel "Problem 5 - test4 " p5_test4,
                   TestLabel "Problem 5 - test5 " p5_test5
                 ] 
                  

-- shortcut to run the tests
run = runTestTT  tests