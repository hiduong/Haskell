{-Haskell HW2 HUnit test cases
 Please add at least 2 additional tests for each problem-}

-- Added 4 additional tests for each problem, testing boundaries, the two trees declared in HW2.hs is also tested here
-- Hien Duong

module HW2SampleTests
    where

import Test.HUnit
import Data.Char
import Data.List (sort)
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

-- Third Tree
{- 
             i
           /   \
          c     h
         / \   / \
        a   b f   g
             / \
            d   e
-}

tree3 = NODE "I" (NODE "C" (LEAF "A") (LEAF "B")) (NODE "H" (NODE "F" (LEAF "D") (LEAF "E")) (LEAF"G"))


-- Sample Tree Integer examples given in the assignment prompt; make sure to provide your own tree examples for both tree data types
-- Your trees should have minimum 4 levels. 
t1 =  NODE 
         "Science" 
         (NODE "and" (LEAF "School")(NODE 
                                      "Engineering" 
                                      (LEAF "of") 
                                      (LEAF "Electrical"))) 
          (LEAF "Computer")

t2 = NODE 1 (NODE 2 (NODE 3 (LEAF 4) (LEAF 5)) (LEAF 6)) (NODE 7 (LEAF 8) (LEAF 9))

t3  = NODE 1 (NODE 2 (NODE 3 (LEAF 2) (LEAF 5)) (LEAF 1)) (NODE 1 (LEAF 8) (LEAF 5))
                                                                
left = NODE 1 (NODE 2 (NODE 3 (LEAF 4) (LEAF 5)) (LEAF 6)) (NODE 7 (LEAF 8) (LEAF 9))
right = NODE 1 (NODE 2 (LEAF 3) (LEAF 6)) (NODE 7 (NODE 8 (LEAF 10) (LEAF 11)) (LEAF 9))

l1 = LEAF "1"
l2 = LEAF "2"
l3 = LEAF "3"
l4 = LEAF "4"
n1 = NODE "5" l1 l2
n2 = NODE "6" n1 l3
t4 = NODE "7" n2 l4

-- TESTING PART 1
-- GIVEN TESTS
p1a_test1 = TestCase (assertEqual "intersect [2,2,5,6,6,8,9] [1,3,2,2,4,4,5,7,8,10] " (sort [2,5,8])  (sort (intersect [2,2,5,6,6,8,9] [1,3,2,2,4,4,5,7,8,10])) ) 
p1a_test2 = TestCase (assertEqual "intersect [5,6,7,8,9] [8,8,10,10,11,12,5] " (sort [5,8])  (sort (intersect [5,6,7,8,9] [8,8,10,10,11,12,5])) ) 
p1a_test3 = TestCase (assertEqual "intersect [\"a\",\"b\",\"d\"] [\"c\",\"e\",\"f\",\"g\"] " []  (intersect ["a","b","d"] ["c","e","f","g"]) ) 

p1b_test1 = TestCase (assertEqual "intersectTail [2,2,5,6,6,8,9] [1,3,2,2,4,4,5,7,8,10] " (sort [2,5,8])  (sort (intersectTail [2,2,5,6,6,8,9] [1,3,2,2,4,4,5,7,8,10])) ) 
p1b_test2 = TestCase (assertEqual "intersectTail [5,6,7,8,9] [8,8,10,10,11,12,5] " (sort [5,8])  (sort (intersectTail [5,6,7,8,9] [8,8,10,10,11,12,5])) ) 
p1b_test3 = TestCase (assertEqual "intersectTail [\"a\",\"b\",\"d\"] [\"c\",\"e\",\"f\",\"g\"] " []  (intersectTail ["a","b","d"] ["c","e","f","g"]) ) 

p1c_test1 = TestCase (assertEqual "intersectAll [[1,3,3,4,5,5,6],[3,4,5],[4,4,5,6],[3,5,6,6,7,8]]" (sort [5])  (sort (intersectAll [[1,3,3,4,5,5,6],[3,4,5],[4,4,5,6],[3,5,6,6,7,8]])) )
p1c_test2 = TestCase (assertEqual "intersectAll [[3,4],[-3,-4,3,4],[-3,-4,5,6]] " []  (sort (intersectAll [[3,4],[-3,-4,3,4],[-3,-4,5,6]])) )
p1c_test3 = TestCase (assertEqual "intersectAll [[3,4,5,5,6],[4,5,6],[],[3,4,5]] " []  (sort (intersectAll [[3,4,5,5,6],[4,5,6],[],[3,4,5]])) )

-- MY ADDED TESTS
intersect_test1 = TestCase (assertEqual "intersect [-9223372036854775809,0,0,1] [-9223372036854775809,2,3,4] " [-9223372036854775809]  (intersect [-9223372036854775809,0,0,1] [-9223372036854775809,2,3,4]) ) 
intersect_test2 = TestCase (assertEqual "intersect [\"Hien\",\"Duong\"] [\"Hien\",\"CS355\",\"Duong\",\"Duong\",\"Hien\"] " (sort ["Hien","Duong"]) (sort (intersect ["Hien","Duong"] ["Hien","CS355","Duong","Duong","Hien"])))
intersect_test3 = TestCase (assertEqual "intersect [] [7,8,9,10,11,12] " [] (intersect [] [7,8,9,10,11,12]))
intersect_test4 = TestCase (assertEqual "intersect [0,9223372036854775809,0,1,3,0,-1,0] [-1,0,1,2,3,9223372036854775809,0] " [-1,0,1,3,9223372036854775809] (sort (intersect [0,9223372036854775809,0,1,3,0,-1,0] [-1,0,1,2,3,9223372036854775809,0])))

intersectTail_test1 = TestCase (assertEqual "intersectTail [-9223372036854775807,0,0,1] [-9223372036854775807,2,3,4] " [-9223372036854775807]  (intersectTail [-9223372036854775807,0,0,1] [-9223372036854775807,2,3,4]) ) 
intersectTail_test2 = TestCase (assertEqual "intersectTail [\"Hien\",\"Duong\"] [\"Hien\",\"CS355\",\"Duong\",\"Duong\",\"Hien\"] " (sort ["Hien","Duong"]) (sort (intersectTail ["Hien","Duong"] ["Hien","CS355","Duong","Duong","Hien"])))
intersectTail_test3 = TestCase (assertEqual "intersectTail [] [7,8,9,10,11,12] " [] (intersectTail [] [7,8,9,10,11,12]))
intersectTail_test4 = TestCase (assertEqual "intersectTail [0,9223372036854775808,0,1,3,0,-1,0] [-1,0,1,2,3,9223372036854775808,0] " [-1,0,1,3,9223372036854775808] (sort (intersectTail [0,9223372036854775808,0,1,3,0,-1,0] [-1,0,1,2,3,9223372036854775808,0])))

intersectAll_test1 = TestCase (assertEqual "intersectAll [[-9223372036854775809],[-9223372036854775809],[-9223372036854775809],[-9223372036854775809]] " [-9223372036854775809]  (sort (intersectAll [[-9223372036854775809],[-9223372036854775809],[-9223372036854775809],[-9223372036854775809]])) )
intersectAll_test2 = TestCase (assertEqual "intersectAll [[9223372036854775809,1,2,3],[9223372036854775809,1],[9223372036854775809,2,1],[9223372036854775809,1,0,2]] " [1,9223372036854775809]  (sort (intersectAll [[9223372036854775809,1,2,3],[9223372036854775809,1],[9223372036854775809,2,1],[9223372036854775809,1,0,2]])) )
intersectAll_test3 = TestCase (assertEqual "intersectAll [[\"h\",\"i\",\"e\",\"n\"], [\"i\",\"e\"], [\"i\",\"h\",\"e\"]] " (sort ["i","e"])  (sort (intersectAll [["h","i","e","n"], ["i","e"], ["i","h","e"]])) )
intersectAll_test4 = TestCase (assertEqual "intersectAll [[\"hien\",\"duong\",\"is\",\"cool\"], [\"hien\",\"cs355\"], [\"hien\",\"duong\",\"bye\"]] " (sort ["hien"])  (sort (intersectAll [["hien","duong","is","cool"], ["hien","cs355"], ["hien","duong","bye"]])) )

-- TESTING PART 2
-- GIVEN TESTS
p2_test1 = TestCase (assertEqual "partition (\\x -> (x<=4)) [1,7,4,5,3,8,2,3] " ([1,4,3,2,3],[7,5,8])  (partition (\x -> (x<=4)) [1,7,4,5,3,8,2,3]) )
p2_test2 = TestCase (assertEqual "partition null [[1,2],[1],[],[5],[],[6,7,8]] " ([[],[]],[[1,2],[1],[5],[6,7,8]])  (partition null [[1,2],[1],[],[5],[],[6,7,8]]) )
p2_test3 = TestCase (assertEqual "partition (elem 1) [[1,2],[1],[],[5],[],[6,7,8]] " ([[1,2],[1]],[[],[5],[],[6,7,8]])  (partition (elem 1) [[1,2],[1],[],[5],[],[6,7,8]] ) )
  
-- MY ADDED TESTS
partition_test1 = TestCase (assertEqual "partition (\\x -> (x > -1)) [1,2,3,4,5,6,7,8,9,10] " ([1,2,3,4,5,6,7,8,9,10],[])  (partition (\x -> (x > -1)) [1,2,3,4,5,6,7,8,9,10] ) )
partition_test2 = TestCase (assertEqual "partition (\\x -> (x < 0)) [1,2,3,4,5,6,7,8,9,10] " ([],[1,2,3,4,5,6,7,8,9,10])  (partition (\x -> (x < 0)) [1,2,3,4,5,6,7,8,9,10] ) )
partition_test3 = TestCase (assertEqual "partition (elem \"hien\") [[\"hien\",\"duong\"],[\"355\",\"is\",\"cool\"]] " ([["hien","duong"]],[["355","is","cool"]])  (partition (elem "hien") [["hien","duong"],["355","is","cool"]] ) )
partition_test4 = TestCase (assertEqual "partition (elem \"a\") [[\"a\",\"b\",\"c\"],[\"a\",\"c\",\"d\"]] " ([["a","b","c"],["a","c","d"]],[])  (partition (elem "a") [["a","b","c"],["a","c","d"]] ) )

-- TESTING PART 3
-- GIVEN TESTS
p3a_test1 = TestCase (assertEqual "sumL [[1,2,3],[4,5],[6,7,8,9],[]]" 45 (sumL [[1,2,3],[4,5],[6,7,8,9],[]]) ) 
p3a_test2 = TestCase (assertEqual "sumL [[10,10],[10,10,10],[10]]" 60 (sumL [[10,10],[10,10,10],[10]]) ) 
p3a_test3 = TestCase (assertEqual "sumL [[]]" 0 (sumL [[]]) ) 

p3b_test1 = TestCase (assertEqual "sumMaybe [[(Just 1),(Just 2),(Just 3)],[(Just 4),(Just 5)],[(Just 6),Nothing ],[],[Nothing ]]" (Just 21) (sumMaybe [[(Just 1),(Just 2),(Just 3)],[(Just 4),(Just 5)],[(Just 6),Nothing ],[],[Nothing ]]) )
p3b_test2 = TestCase (assertEqual "sumMaybe [[(Just 10),Nothing],[(Just 10), (Just 10), (Just 10),Nothing,Nothing]]" (Just 40) (sumMaybe [[(Just 10),Nothing],[(Just 10), (Just 10), (Just 10),Nothing,Nothing]]) )
p3b_test3 = TestCase (assertEqual "sumMaybe [[Nothing ]]" (Nothing) (sumMaybe [[Nothing ]]) )

p3c_test1 = TestCase (assertEqual "sumEither [[IString \"1\",IInt 2,IInt 3],[IString \"4\",IInt 5],[IInt 6,IString \"7\"],[],[IString \"8\"]]" (IInt 36) (sumEither [[IString "1",IInt 2,IInt 3],[IString "4",IInt 5],[IInt 6,IString "7"],[],[IString "8"]]) )
p3c_test2 = TestCase (assertEqual "sumEither [[IString \"10\" , IInt 10],[],[IString \"10\"],[]]" (IInt 30) (sumEither [[IString "10" , IInt 10],[],[IString "10"],[]]) )
p3c_test3 = TestCase (assertEqual "sumEither  [[]]" (IInt 0) (sumEither  [[]]) )

-- MY ADDED TESTS 
sumL_test1 = TestCase (assertEqual "sumL [[-9223372036854775807],[-2],[]] " (-9223372036854775809) (sumL [[-9223372036854775807],[-2],[]])) 
sumL_test2 = TestCase (assertEqual "sumL [[9223372036854775807],[2],[]] " (9223372036854775809) (sumL [[9223372036854775807],[2],[]])) 
sumL_test3 = TestCase (assertEqual "sumL [[1.2,2,4.5,6.7,8.9],[1.1,2,3,4,5],[1]] " 39.400000000000006 (sumL [[1.2,2,4.5,6.7,8.9],[1.1,2,3,4,5],[1]])) 
sumL_test4 = TestCase (assertEqual "sumL [[],[1,-1,1,-1],[],[2,-2,2,-2],[]] " 0 (sumL [[],[1,-1,1,-1],[],[2,-2,2,-2],[]])) 

sumMaybe_test1 = TestCase (assertEqual "sumMaybe [[(Just (-9223372036854775807))],[(Just (-2))],[Nothing]] " (Just (-9223372036854775809)) (sumMaybe [[(Just (-9223372036854775807))],[(Just (-2))],[Nothing]]))
sumMaybe_test2 = TestCase (assertEqual "sumMaybe [[(Just -9223372036854775807)],[(Just 2)],[Nothing]] " (Just 9223372036854775809) (sumMaybe [[(Just 9223372036854775807)],[(Just 2)],[Nothing]]))
sumMaybe_test3 = TestCase (assertEqual "sumMaybe [[(Just (-1)),(Just 1),(Just (-1))],[(Just 1),(Just (-1))],[Nothing]] " (Just (-1)) (sumMaybe [[(Just (-1)),(Just 1),(Just (-1))],[(Just 1),(Just (-1))],[Nothing]]))
sumMaybe_test4 = TestCase (assertEqual "sumMaybe [[(Nothing)],[(Nothing)],[(Nothing)]] " (Nothing) (sumMaybe [[(Nothing)],[(Nothing)],[(Nothing)]]))

sumEither_test1 = TestCase (assertEqual "sumEither [[IInt (-9223372036854775806)],[],[IString \"(-1)\"]] " (IInt (-9223372036854775807)) (sumEither  [[IInt (-9223372036854775806)],[],[IString "-1"]]) )
sumEither_test2 = TestCase (assertEqual "sumEither [[IInt (9223372036854775806)],[],[IString \"1\"]] " (IInt 9223372036854775807) (sumEither  [[IInt (9223372036854775806)],[],[IString "1"]]) )
sumEither_test3 = TestCase (assertEqual "sumEither [[IString \"1\",IString \"-1\"],[IString \"1\", IString \"-1\"],[IInt 0]] " (IInt 0) (sumEither  [[IString "1",IString "-1"],[IString "1", IString "-1"],[IInt 0]]) )
sumEither_test4 = TestCase (assertEqual "sumEither [[IInt 0, IInt 0],[IString \"0\", IString \"0\"],[IInt 0]] " (IInt 0) (sumEither  [[IInt 0,IInt 0, IInt 0],[IString "0", IString "0"],[IInt 0]]) )

--TESTING PART 4
-- GIVEN TESTS
p4a_test1 = TestCase (assertEqual "depthScan t1"  ["School","of","Electrical","Engineering","and","Computer","Science"] (depthScan t1) ) 
p4a_test2 = TestCase (assertEqual "depthScan t2" [4,5,3,6,2,8,9,7,1] (depthScan t2) ) 

p4b_test1 = TestCase (assertEqual "depthSearch t3 1" 3 (depthSearch t3 1) ) 
p4b_test2 = TestCase (assertEqual "depthSearch t3 5" 4 (depthSearch t3 5) )
p4b_test3 = TestCase (assertEqual "depthSearch t3 4" (-1) (depthSearch t3 4) )

addedTree = NODE 2 (NODE 4 (NODE 6 (LEAF 4) (LEAF 5)) (LEAF 12)) (NODE 14 (NODE 16 (LEAF 10) (LEAF 11)) (LEAF 18))
p4c_test1 = TestCase (assertEqual ("addTrees "++ (show left) ++ (show right)) addedTree  (addTrees left right) ) 

-- MY ADDED TESTS
depthScan_test1 = TestCase (assertEqual "depthScan tree1 " [1,3,2,5,4,10,13,22,30,25,20,12,9] (depthScan tree1) ) 
depthScan_test2 = TestCase (assertEqual "depthScan tree2 " [5,8,8,5,5,2,1,1,3,13,100,7,4,5,8,5,2] (depthScan tree2) )
depthScan_test3 = TestCase (assertEqual "depthScan tree3 " ["A","B","C","D","E","F","G","H","I"] (depthScan tree3) ) 
depthScan_test4 = TestCase (assertEqual "depthScan (LEAF \"Done\") " ["Done"] (depthScan (LEAF "Done")))

depthSearch_test1 = TestCase (assertEqual "depthSearch tree1 9 " (1) (depthSearch tree1 9) )
depthSearch_test2 = TestCase (assertEqual "depthSearch tree1 22 " (5) (depthSearch tree1 22) )
depthSearch_test3 = TestCase (assertEqual "depthSearch tree2 8 " (5) (depthSearch tree2 8) )
depthSearch_test4 = TestCase (assertEqual "depthSearch tree2 100 " (6) (depthSearch tree2 100) )

{- result of adding tree 1 and tree 2
            11
          /    \
         5      17
        / \     / \
       7   7   15  28
      / \     / \   / \
     9   8   1   4  13 25
    / \          /\    / \
   5   8        3  7  22 30
                  / \
                 13 100 
-} 
tree1andtree2 = NODE 11 (NODE 5 (NODE 7 (NODE 9 (LEAF 5) (LEAF 8)) (LEAF 8)) (LEAF 7)) (NODE 17 (NODE 15 (LEAF 1) (NODE 4 (LEAF 3) (NODE 7 (LEAF 13) (LEAF 100)))) (NODE 28 (LEAF 13) (NODE 25 (LEAF 22) (LEAF 30))))

addTrees_test1 = TestCase (assertEqual ("addTrees "++ (show tree1) ++ (show tree2)) tree1andtree2  (addTrees tree1 tree2) ) 
addTrees_test2 = TestCase (assertEqual "addTrees (LEAF 1) (LEAF (-2)) " (LEAF (-1)) (addTrees (LEAF 1) (LEAF (-2)))) 
addTrees_test3 = TestCase (assertEqual "addTrees (NODE 5 (LEAF 2) (LEAF 3)) (LEAF 5) " (NODE 10 (LEAF 2) (LEAF 3)) (addTrees (NODE 5 (LEAF 2) (LEAF 3)) (LEAF 5))) 
addTrees_test4 = TestCase (assertEqual "addTrees (NODE 1.5 (LEAF 1) (LEAF 0)) (NODE 2.1 (LEAF -1) (LEAF 0)) " (NODE 3.6 (LEAF 0.0) (LEAF 0.0)) (addTrees (NODE 1.5 (LEAF 1) (LEAF 0)) (NODE 2.1 (LEAF (-1)) (LEAF 0)))) 

tests = TestList [ TestLabel "Problem 1a - test1 " p1a_test1,
                   TestLabel "Problem 1a - test2 " p1a_test2,
                   TestLabel "Problem 1a - test3 " p1a_test3,
                   TestLabel "Problem 1a - myTests1 " intersect_test1,
                   TestLabel "Problem 1a - myTests2 " intersect_test2, 
                   TestLabel "Problem 1a - myTests3 " intersect_test3, 
                   TestLabel "Problem 1a - myTests4 " intersect_test4,                    
                   TestLabel "Problem 1b - test1 " p1b_test1,
                   TestLabel "Problem 1b - test2 " p1b_test2,                   
                   TestLabel "Problem 1b - test3 " p1b_test3, 
                   TestLabel "Problem 1b - myTests1 " intersectTail_test1,
                   TestLabel "Problem 1b - myTests2 " intersectTail_test2, 
                   TestLabel "Problem 1b - myTests3 " intersectTail_test3, 
                   TestLabel "Problem 1b - myTests4 " intersectTail_test4,                                      
                   TestLabel "Problem 1c - test1 " p1c_test1,
                   TestLabel "Problem 1c - test2 " p1c_test2,
                   TestLabel "Problem 1c - test3 " p1c_test3,  
                   TestLabel "Problem 1c - myTests1 " intersectAll_test1,
                   TestLabel "Problem 1c - myTests2 " intersectAll_test2, 
                   TestLabel "Problem 1c - myTests3 " intersectAll_test3, 
                   TestLabel "Problem 1c - myTests4 " intersectAll_test4,                                     
                   TestLabel "Problem 2  - test1 " p2_test1,
                   TestLabel "Problem 2  - test2 " p2_test2,  
                   TestLabel "Problem 2  - test3 " p2_test3,
                   TestLabel "Problem 2 - myTests1 " partition_test1,
                   TestLabel "Problem 2 - myTests2 " partition_test2, 
                   TestLabel "Problem 2 - myTests3 " partition_test3, 
                   TestLabel "Problem 2 - myTests4 " partition_test4,
                   TestLabel "Problem 3a - test1 " p3a_test1,
                   TestLabel "Problem 3a - test2 " p3a_test2,  
                   TestLabel "Problem 3a - test3 " p3a_test3,  
                   TestLabel "Problem 3a - myTests1 " sumL_test1,
                   TestLabel "Problem 3a - myTests2 " sumL_test2, 
                   TestLabel "Problem 3a - myTests3 " sumL_test3, 
                   TestLabel "Problem 3a - myTests4 " sumL_test4,                  
                   TestLabel "Problem 3b - test1 " p3b_test1,
                   TestLabel "Problem 3b - test2 " p3b_test2,
                   TestLabel "Problem 3b - test3 " p3b_test3,
                   TestLabel "Problem 3b - myTests1 " sumMaybe_test1,
                   TestLabel "Problem 3b - myTests2 " sumMaybe_test2, 
                   TestLabel "Problem 3b - myTests3 " sumMaybe_test3, 
                   TestLabel "Problem 3b - myTests4 " sumMaybe_test4, 
                   TestLabel "Problem 3c - test1 " p3c_test1,
                   TestLabel "Problem 3c - test2 " p3c_test2,
                   TestLabel "Problem 3c - test3 " p3c_test3,
                   TestLabel "Problem 3c - myTests1 " sumEither_test1,
                   TestLabel "Problem 3c - myTests2 " sumEither_test2, 
                   TestLabel "Problem 3c - myTests3 " sumEither_test3, 
                   TestLabel "Problem 3c - myTests4 " sumEither_test4,
                   TestLabel "Problem 4a - test1 " p4a_test1,
                   TestLabel "Problem 4a - test2 " p4a_test2,
                   TestLabel "Problem 4a - myTests1 " depthScan_test1,
                   TestLabel "Problem 4a - myTests2 " depthScan_test2, 
                   TestLabel "Problem 4a - myTests3 " depthScan_test3, 
                   TestLabel "Problem 4a - myTests4 " depthScan_test4,
                   TestLabel "Problem 4b - test1 " p4b_test1,
                   TestLabel "Problem 4b - test2 " p4b_test2,
                   TestLabel "Problem 4b - test3 " p4b_test3,
                   TestLabel "Problem 4b - myTests1 " depthSearch_test1,
                   TestLabel "Problem 4b - myTests2 " depthSearch_test2, 
                   TestLabel "Problem 4b - myTests3 " depthSearch_test3, 
                   TestLabel "Problem 4b - myTests4 " depthSearch_test4,
                   TestLabel "Problem 4c - test1 " p4c_test1,
                   TestLabel "Problem 4c - myTests1 " addTrees_test1,
                   TestLabel "Problem 4c - myTests2 " addTrees_test2,
                   TestLabel "Problem 4c - myTests3 " addTrees_test3,
                   TestLabel "Problem 4c - myTests4 " addTrees_test4
                 ] 
                  

-- shortcut to run the tests
run = runTestTT  tests