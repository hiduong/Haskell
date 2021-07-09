{- Two useful functions in the HUnit package are assertEqual and assertBool.
The arguments to 'assertEqual' are:
      a descriptive string
      the expected value
      the value being tested
The arguments to 'assertBool' are:
      a descriptive string
      the boolean value being tested
-}

module HUnitSampleTests
    where

import Test.HUnit
import HW1

-- testing function exists--
exists_test1 = TestCase (assertEqual "exists 1 []" False (exists 1 []) ) 
exists_test2 = TestCase (assertEqual "exists 1 [1,2,3]" True (exists 1 [1,2,3]) ) 
exists_test3 = TestCase (assertEqual "exists [1] [[1]]" True (exists [1] [[1]]) ) 
exists_test4 = TestCase (assertEqual "exists [1] [[3],[5]]" False (exists [1] [[3],[5]]) ) 
exists_test5 = TestCase (assertEqual "exists '3' CptS355" True (exists '3' "CptS355") ) 
exists_test6 = TestCase (assertEqual "exists (1,'a') [(2,'b'),(1,'a'),(3,'c')]" True (exists (1,'a') [(2,'b'),(1,'a'),(3,'c')]) ) 

-- testing function countInList--
countInList_test1 = TestCase (assertEqual "countInList 5 [3,5,5,-,4,5,1]" 3 (countInList "5" ["3","5","5","-","4","5","1"]) )
countInList_test2 = TestCase (assertEqual "countInList 5 []" 0 (countInList "5" [] ) )
countInList_test3 = TestCase (assertEqual "countInList  True [True, False, False, False, True, True, True] " 4 (countInList  True [True, False, False, False, True, True, True]  ) )  
countInList_test4 = TestCase (assertEqual "countInList  [] [[],[1,2],[3,2],[5,6,7],[8],[]] " 2 (countInList  [] [[],[1,2],[3,2],[5,6,7],[8],[]]  ) )
countInList_test5 = TestCase (assertEqual "countInList dog [dog,cat,dog,dog,dog,dog,cat,monkey]" 5 (countInList "dog" ["dog","cat","dog","dog","dog","dog","cat","monkey"]) )
countInList_test6 = TestCase (assertEqual "countInList (1,'a') [(2,'b'),(1,'a'),(3,'c')]" 1 (countInList (1,'a') [(2,'b'),(1,'a'),(3,'c')]) )

--testing function listDiff--
listDiff_test1 = TestCase (assertEqual "listDiff [a,b,c] [b]"  ["a","c"] (listDiff ["a","b","c"] ["b"]) ) 
listDiff_test2 = TestCase (assertEqual "listDiff [1,2,3] [1,1,2]"  [3] (listDiff [1,2,3] [1,1,2]) ) 
listDiff_test3 = TestCase (assertEqual "listDiff [1,2,2,3,3,3] [1,1,2,3]"  [2,3,3] (listDiff [1,2,2,3,3,3] [1,1,2,3] ) ) 
listDiff_test4 = TestCase (assertEqual "listDiff [[2,3],[1,2],[2,3]] [[1],[2,3]] "  [[2,3],[1,2]] (listDiff [[2,3],[1,2],[2,3]] [[1],[2,3]] ) ) 
listDiff_test5 = TestCase (assertEqual "listDiff [1,2,3] [] "  [1,2,3] (listDiff [1,2,3] [] ) ) 
listDiff_test6 = TestCase (assertEqual "listDiff [cpts321,cpts355,cpts460,cpts360,cpts355] [cpts355]"  ["cpts321","cpts460","cpts360"] (listDiff ["cpts321","cpts355","cpts460","cpts360","cpts355"] ["cpts355","cpts355"]) ) 

--testing function firstN--
firstN_test1 = TestCase (assertEqual "firstN [a, b, c, x, y] 3" ["a", "b", "c"] (firstN ["a", "b", "c", "x", "y"] 3) ) 
firstN_test2 = TestCase (assertEqual "firstN [1,2,3,4,5,6,7] 4" [1,2,3,4] (firstN [1,2,3,4,5,6,7] 4) )
firstN_test3 = TestCase (assertEqual "firstN [1,2,3,4,5,6,7] 10" [1,2,3,4,5,6,7] (firstN [1,2,3,4,5,6,7] 10) )
firstN_test4 = TestCase (assertEqual "firstN [[1,2,3],[4,5],[6],[],[7,8],[]] 4" [[1,2,3],[4,5],[6],[]] ( firstN [[1,2,3],[4,5],[6],[],[7,8],[]] 4) )
firstN_test5 = TestCase (assertEqual "firstN [] 5 " "" (firstN [] 5 ) )
firstN_test6 = TestCase (assertEqual "firstN [hello,goodbye,bye,hi,ok] 2" ["hello", "goodbye"] (firstN ["hello","goodbye","bye","hi","ok"] 2) )

--testing function busFiinder--
buses = [("Lentil",["Chinook", "Orchard", "Valley", "Emerald","Providence", "Stadium", "Main",
 "Arbor", "Sunnyside", "Fountain", "Crestview", "Wheatland", "Walmart", "Bishop",
 "Derby", "Dilke"]),
 ("Wheat",["Chinook", "Orchard", "Valley", "Maple","Aspen", "TerreView", "Clay",
 "Dismores", "Martin", "Bishop", "Walmart", "PorchLight", "Campus"]),
 ("Silver",["TransferStation", "PorchLight", "Stadium", "Bishop","Walmart", "Shopco",
 "RockeyWay"]),
 ("Blue",["TransferStation", "State", "Larry", "TerreView","Grand", "TacoBell",
 "Chinook", "Library"]),
 ("Gray",["TransferStation", "Wawawai", "Main", "Sunnyside","Crestview", "CityHall",
 "Stadium", "Colorado"])
 ] 

busFinder_test1 = TestCase (assertEqual "busFinder Walmart buses" ["Lentil","Wheat","Silver"] (busFinder "Walmart" buses) ) 
busFinder_test2 = TestCase (assertEqual "busFinder Shopco buses" ["Silver"] (busFinder "Shopco" buses) ) 
busFinder_test3 = TestCase (assertEqual "busFinder Main buses" ["Lentil","Gray"] (busFinder "Main" buses) ) 
busFinder_test4 = TestCase (assertEqual "busFinder Beasley buses" [] (busFinder "Beasley" buses) ) 
busFinder_test5 = TestCase (assertEqual "busFinder hien [(name,[hien,duong,hello,world]),(age,[hien,duong,hello,world]),(hello,[hello,world])]" ["name","age"] (busFinder "hien" [("name",["hien","duong","hello","world"]),("age",["hien","duong","hello","world"]),("hello",["hello","world"])]) ) 
busFinder_test6 = TestCase (assertEqual "busFinder 1 [(2,[1,2,3,4,5,6]),(3,[2,3,4,5,6,7])]" [2] (busFinder 1 [(2,[1,2,3,4,5,6]),(3,[2,3,4,5,6,7])]))

--testing function cumulativeSums--
cumulativeSums_test1 = TestCase (assertEqual "cumulativeSums [1,2,3,4,5,6,7,8,9,10]" [1,3,6,10,15,21,28,36,45,55] (cumulativeSums [1,2,3,4,5,6,7,8,9,10]) ) 
cumulativeSums_test2 = TestCase (assertEqual "cumulativeSums [5,5,5,5,5,5,5]" [5,10,15,20,25,30,35] (cumulativeSums [5,5,5,5,5,5,5]) )
cumulativeSums_test3 = TestCase (assertEqual "cumulativeSums [1,2,3,4,-4,-3,-2]" [1,3,6,10,6,3,1] (cumulativeSums [1,2,3,4,-4,-3,-2]) )
cumulativeSums_test4 = TestCase (assertEqual "cumulativeSums []" [] (cumulativeSums []) )
cumulativeSums_test5 = TestCase (assertEqual "cumulativeSums [100,200,300,400,500,600]" [100,300,600,1000,1500,2100] (cumulativeSums [100,200,300,400,500,600]) )
cumulativeSums_test6 = TestCase (assertEqual "cumulativeSums [1,-1,2,-2,3,-3,100,-100]" [1,0,2,0,3,0,100,0] (cumulativeSums [1,-1,2,-2,3,-3,100,-100]) )

--testing function groupNleft--
groupNleft_test1 = TestCase (assertEqual "groupNleft 3 [1, 2, 3, 4, 5, 6, 7, 8]" [[1,2,3],[4,5,6],[7,8]] (groupNleft 3 [1, 2, 3, 4, 5, 6, 7, 8]) ) 
groupNleft_test2 = TestCase (assertEqual "groupNleft 5 [1, 2, 3, 4, 5, 6, 7, 8]" [[1,2,3,4,5],[6,7,8]]  (groupNleft 5 [1, 2, 3, 4, 5, 6, 7, 8]) ) 
groupNleft_test3 = TestCase (assertEqual "groupNleft 2 [(1,a),(2,b),(3,c),(4,d),(5,e),(6,f)]" [[(1,"a"),(2,"b")],[(3,"c"),(4,"d")],[(5,"e"),(6,"f")]] (groupNleft 2 [(1,"a"),(2,"b"),(3,"c"),(4,"d"),(5,"e"),(6,"f")]) ) 
groupNleft_test4 = TestCase (assertEqual "groupNleft 2 [1, 2, 3, 4, 5, 6, 7, 8]" [[1,2],[3,4],[5,6],[7,8]] (groupNleft 2 [1, 2, 3, 4, 5, 6, 7, 8]) ) 
groupNleft_test5 = TestCase (assertEqual "groupNleft 3 [-1, 2, -3, -4, 5 , -6 , -7]" [[-1,2,-3],[-4,5,-6],[-7]] (groupNleft 3 [-1, 2, -3, -4, 5 , -6 , -7]) )
groupNleft_test6 = TestCase (assertEqual "groupNleft 2 ['a','b','c','d','e','f','g']" [['a','b'],['c','d'],['e','f'],['g']] (groupNleft 2 ['a','b','c','d','e','f','g']) ) 

tests = TestList [ TestLabel "exists - test1 " exists_test1,
                   TestLabel "exists - test2 " exists_test2,
                   TestLabel "exists - test3 " exists_test3,
                   TestLabel "exists - test4 " exists_test4,
                   TestLabel "exists - test5 " exists_test5,
                   TestLabel "exists - test6 " exists_test6,
                   TestLabel "countInList - test1 " countInList_test1,
                   TestLabel "countInList - test2 " countInList_test2,
                   TestLabel "countInList - test3 " countInList_test3,
                   TestLabel "countInList - test4 " countInList_test4,
                   TestLabel "countInList - test5 " countInList_test5,
                   TestLabel "countInList - test6 " countInList_test6,
                   TestLabel "listDiff - test1 " listDiff_test1,
                   TestLabel "listDiff - test2 " listDiff_test2,
                   TestLabel "listDiff - test3 " listDiff_test3,
                   TestLabel "listDiff - test4 " listDiff_test4,
                   TestLabel "listDiff - test5 " listDiff_test5,
                   TestLabel "listDiff - test6 " listDiff_test6,
                   TestLabel "firstN - test1 " firstN_test1,
                   TestLabel "firstN - test2 " firstN_test2,
                   TestLabel "firstN - test3 " firstN_test3,
                   TestLabel "firstN - test4 " firstN_test4,
                   TestLabel "firstN - test5 " firstN_test5,
                   TestLabel "firstN - test6 " firstN_test6,
                   TestLabel "busFinder - test1 " busFinder_test1,
                   TestLabel "busFinder - test2 " busFinder_test2,
                   TestLabel "busFinder - test3 " busFinder_test3,
                   TestLabel "busFinder - test4 " busFinder_test4,
                   TestLabel "busFinder - test5 " busFinder_test5,
                   TestLabel "busFinder - test6 " busFinder_test6,
                   TestLabel "cumulativeSums - test1 " cumulativeSums_test1,
                   TestLabel "cumulativeSums - test2 " cumulativeSums_test2,
                   TestLabel "cumulativeSums - test3 " cumulativeSums_test3,
                   TestLabel "cumulativeSums - test4 " cumulativeSums_test4,
                   TestLabel "cumulativeSums - test5 " cumulativeSums_test5,
                   TestLabel "cumulativeSums - test6 " cumulativeSums_test6,
                   TestLabel "groupNleft - test1 " groupNleft_test1,
                   TestLabel "groupNleft - test2 " groupNleft_test2,
                   TestLabel "groupNleft - test3 " groupNleft_test3,
                   TestLabel "groupNleft - test4 " groupNleft_test4,
                   TestLabel "groupNleft - test5 " groupNleft_test5,
                   TestLabel "groupNleft - test6 " groupNleft_test6
                 ] 
                  

-- shortcut to run the tests
run = runTestTT  tests