-- CptS 355 - Spring 2020 Assignment 2
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework

-- Hien Duong

module HW2
     where

-- exists function from HW1 to use for part 1
exists :: Eq t => t -> [t] -> Bool
exists x [] = False -- base case, if gone through whole list and still not true, that means that it doesnt exist, return false
exists x (y : ys) | x == y = True -- if value exists return true
                  | otherwise = exists x ys -- call the function again with the tail of the list
          
{- intersect & intersectTail & intersectAll - 22%-}
--intersect

intersect :: Eq a => [a] -> [a] -> [a]
intersect [][] = [] -- if both list empty return empty list
intersect x [] = [] -- if first list empty return empty list
intersect [] y = [] -- our break case if the first list is empty we will be returning the intersection list, or if the first list input is empty we just return the empty list
intersect (x : xs) y | (exists x xs == True) && (exists x y == True) = intersect xs y -- If the current element of the first list appears again and it appears in the second list, simply go to next element of first list, because we don't want duplicates, so only the last recurring element will be inputted.
                     | (exists x xs == False) && (exists x y == True) = x : intersect xs y -- if it is the last recurring element in the first list and it exists in the second list cons it to the list we will be returning
                     | otherwise = intersect xs y -- otherwise go to next element in the first list

--intersectTail
intersectTail :: Eq a => [a] -> [a] -> [a]
intersectTail [][] = [] -- if both list empty return empty list
intersectTail x [] = [] -- if first list empty return empty list
intersectTail [] y = [] -- if second list empty return empty list
intersectTail x y = let
                    intersectTailHelper acc [] y = acc -- Tail recursive function by having an accumulator that will hold the intersection of the two lists
                    intersectTailHelper acc (x : xs) y | (exists x xs == True) && (exists x y == True) = intersectTailHelper acc xs y -- if there is a duplicate in the first list and that value exists in the second list, we will only cons the last reocurrence of the duplicate to the accumulator. So this will take care of dups, so if this is true just go to the next element.
                                                       | (exists x xs == False) && (exists x y == True) = intersectTailHelper (x:acc) xs y -- if this is the last occurence of the duplicate and it exists in the second list cons it to the accumulator and go to the next element
                                                       | otherwise = intersectTailHelper acc xs y -- otherwise just go to the next element
                    in intersectTailHelper [] x y -- caling helper function with accumulator starting at empty list [] and the two lists to intersect, on return we will return the accumulator
     
--intersectAll
intersectAll:: Ord a => [[a]] -> [a] 
intersectAll (x:xs) = foldr (intersect) x xs
{- using [[1,2,3],[1],[1,2,3]] the function above will do,
   xs intersect (xs intersect (xs intersect x))
   so it does intersect [1,2,3] [1] = [1]
   then intersect [1] [1] = [1]
   then intersect [1] [1,2,3] = [1] -}

{-2 - partition - 10%-}
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition op [] = ([],[]) -- if input list is empty just return empty list
partition op x = (filter op x, filter (not . op) x) 
{- Since filter takes a predicate and filters the list based on the predicate
   in order to filter everything NOT part of the predicate we would have to negate it, ~predicate
   thus, we can simply return (filtered list of true, filtered list of false), since filter
   returns a list satisfying those conditions -}

{- 3 - sumL, sumMaybe, and sumEither - 27% -}

--sumL
sumL :: (Num b) => [[b]] -> b
sumL [] = 0 -- if input list is empty return empty list
sumL x = let 
         sumLHelper y = foldr (+) 0 y -- helper function that add ups all the elements in a single list using foldr
         in sumLHelper (map sumLHelper x) -- takes the helper function to calculate sum of one list and maps it to the other lists, then calls the helper function again to calculate the total sum  

-- sumMaybe 
sumMaybe :: (Num a) => [[(Maybe a)]] -> Maybe a
sumMaybe [] = Nothing -- if list is empty simply return Nothing
sumMaybe x = let
             -- Using patterns to add Maybe values
             addMaybe :: (Num a) => Maybe a -> Maybe a -> Maybe a
             addMaybe Nothing Nothing = Nothing -- if both inputs are nothing return nothing
             addMaybe Nothing (Just y) = (Just y) -- if first input nothing just return 2nd input
             addMaybe (Just x) Nothing =  (Just x) -- if second input nothing just return the first input
             addMaybe (Just x) (Just y) = (Just (x + y)) -- if both inputs a are Just add them

             sumMaybeHelper x = foldr addMaybe Nothing x -- helper function that add the Maybe values up in a single list
             in sumMaybeHelper(map sumMaybeHelper x) -- using the helper function, map it to all the lists and calculate the total sum

-- sumEither
data IEither  = IString String | IInt Int
                deriving (Show, Read, Eq)

sumEither:: [[IEither]] -> IEither
sumEither [] = IInt 0 -- if the input list is empty just return IInt 0
sumEither x = let
              getInt x = read x::Int -- function to convert string to integer

              -- using patterns to extract values, convert if needed and add them
              convertAndAdd :: IEither -> IEither -> IEither
              convertAndAdd (IString x) (IInt y) = IInt((getInt x) + y) -- If string + int we first want to convert the string
              convertAndAdd (IInt x) (IString y) = IInt(x + (getInt y)) -- if int + string, convert string
              convertAndAdd (IInt x) (IInt y) = IInt(x + y) -- if int + int simply add
              convertAndAdd (IString x) (IString y) = IInt((getInt x) + (getInt y)) -- if string + string, convert both to int and add

              sumEitherHelper y = foldr convertAndAdd (IInt 0) y --helper function that uses foldr to add up elements in a single list
              in sumEitherHelper (map sumEitherHelper x) -- using the helper function, map that to all the lists and calculate the total sum

{-4 - depthScan, depthSearch, addTrees - 37%-}

data Tree a = LEAF a | NODE a (Tree a) (Tree a)
              deriving (Show, Read, Eq)
 
--depthScan
-- Essentially Post Order Traversal so left, right, root
depthScan :: Tree a -> [a]
depthScan (LEAF x) = [x] -- If it's a Leaf return the value
depthScan (NODE t0 t1 t2) = (depthScan t1) ++ (depthScan t2) ++ [t0] -- recursively get value of the left side then right then value of the node into a list

levelOrder :: Tree a -> [a]
levelOrder (LEAF x) = [x]
levelOrder (NODE x y z) = (levelOrder y) ++ [x] ++ (levelOrder z)

preOrder :: Tree a -> [a]
preOrder (LEAF x) = [x]
preOrder (NODE x y z) = [x] ++ (preOrder y) ++ (preOrder z)

--depthSearch
depthSearch :: (Ord p, Num p, Eq a) => Tree a -> a -> p
depthSearch (LEAF x) y = 0
depthSearch (NODE t0 t1 t2) y = let -- this helper function will post order traverse and when it comes back it will check the value of the leaf if it matches the value we want return z, the current level, else return -1
                                helper (LEAF x) y z | y == x = z -- if the leaf value is equal to the value we want return it, if not just return -1
                                                    | otherwise = -1
                                helper (NODE t0 t1 t2) y z | ((t0 == y) == True) && ((t0 `elem` (depthScan t1)) == False) && ((t0 `elem` (depthScan t2)) == False)= z -- This case will essentially check to see if the Node value is the value that we want, BUT since we are post order traversing we have to make sure that the value doesn't already exist in the later sections of the tree, so if it doesn't, return the current level
                                                           | (helper t1 y (z + 1)) == -1 = (helper t2 y (z + 1)) -- in the left side of the tree if the leaf value does not match the value we want go to the right side
                                                           | (helper t2 y (z + 1)) == -1 = (helper t1 y (z + 1)) -- in the right side of the tree if the leaf value does not match the value we want go to the left side
                                                           | otherwise = (helper t1 y (z + 1)) -- else condition if the Node value does not match, we will call the left side of the tree first since this is post order traversal
                                in helper (NODE t0 t1 t2) y 1 -- calling helper function with Tree the value we want and the current level which is 1, the tree will increment level by 1 everything it goes down a level
                              
--addTrees
addTrees :: Num a => Tree a -> Tree a -> Tree a
addTrees (LEAF x) (LEAF y) = LEAF (x + y) -- Add two leaf values and put it into the new tree
addTrees (NODE t1 t2 t3) (LEAF x) = NODE (x + t1) t2 t3 -- Add the value of the node and leaf node and return the new Node
addTrees (LEAF x) (NODE t1 t2 t3) = NODE (x + t1) t2 t3 -- add the value of the node and leaf node and return the new Node
addTrees (NODE x y z) (NODE t1 t2 t3) = NODE (x + t1) (addTrees y t2) (addTrees z t3) -- Add the values of the nodes and recursively call again on the left and right side.

{- 5- Create two trees of type Tree. The height of both trees should be at least 4. Test your functions depthScan, depthSearch, addTrees with those trees. 
The trees you define should be different than those that are given.   -}

-- FIRST TREE 
{-
               9
              /  \
             4    12
            / \   /  \  
           2   5 10  20
          / \        / \
         1   3      13  25
                        / \
                       22  30                 
-} 

tree1 = NODE 9 (NODE 4 (NODE 2 (LEAF 1) (LEAF 3)) (LEAF 5)) (NODE 12 (LEAF 10) (NODE 20 (LEAF 13) (NODE 25 (LEAF 22) (LEAF 30))))

-- SECOND TREE
{-
             2
           /    \
          1      5
         / \    / \
        5   2  5   8
       / \    / \
      8   5  1   4
     / \        / \
    5   8      3   7
                  / \
                 13  100             
-}

tree2 = NODE 2 (NODE 1 (NODE 5 (NODE 8 (LEAF 5) (LEAF 8)) (LEAF 5)) (LEAF 2)) (NODE 5 (NODE 5 (LEAF 1) (NODE 4 (LEAF 3) (NODE 7 (LEAF 13) (LEAF 100)))) (LEAF 8))

{- 
   Tests for the trees above implemented in HW2Tests.hs
-}


lookup' :: Eq a1 => a1 -> [(a1,a2)] ->[a2]
lookup' k [] = []
lookup' k (x:xs) | (fst x) == k = (snd x) : lookup' k xs
                | otherwise = lookup' k xs


func :: Eq a => [[a]] -> a -> [[a]]
func [] x = []
func il y = let
                helper x y | elem y x == Falsem = y:x
                           | otherwise = x
               in map helper il

