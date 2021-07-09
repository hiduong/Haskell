-- CptS 355 - Spring 2020 Assignment 1
-- Please include your name and the names of the students with whom you discussed any of the problems in this homework

-- NAME : Hien Duong

module HW1
     where

lookup :: Eq a1 -> a1 -> [(a1,a2)] ->[a2]
lookup k [] = []
lookup k (x:xs) | (fst x) == k = (snd x) : lookup k xs
                | otherwise = lookup k xs

-- 1a. exists
exists :: Eq t => t -> [t] -> Bool
exists x [] = False -- base case, if gone through whole list and still not true, that means that it doesnt exist, return false
exists x (y : ys) | x == y = True -- if value exists return true
                  | otherwise = exists x ys -- call the function again with the tail of the list

-- 1b. type for exists
-- The type is exists :: Eq t => t -> [t] -> Bool but not exists :: t -> [t] -> Bool because in the function we are compareing two values.
-- The Eq type is needed to make equal(==) and not equal(/=) comparisons between two inputs. As seen in the function above Eq is 
-- needed to make the x == y comparison, without it you would get an error.

-- 1.c countInList
countInList  :: (Num p, Eq t) => t -> [t] -> p
countInList x [] = 0 -- base case, if gone through the whole list will return the count
countInList x (y : ys) | x == y = 1 + countInList x ys -- if value exists increment by 1 and call the function again with tail of list
                       | otherwise = countInList x ys -- call the function again with the tail of the list

-- 2. listDiff
listDiff :: Eq a => [a] -> [a] -> [a]
listDiff [] [] = [] -- if both lists are empty just return an empty list
listDiff x [] = x -- if the second input list is empty just return the first list
listDiff [] y = [] -- base case, if the first list is empty return the difference list
listDiff (x : xs) y | countInList x (x : xs) > countInList x y = x : listDiff xs y -- if the count in the first list is greater than the second list that means that the second list is missing the element so add the difference
                    | otherwise = listDiff xs y -- else just call the function with the tail of the first list

-- 3. firstN
firstN :: (Ord t, Num t) => [a] -> t -> [a]  
firstN [] y = [] -- if list is empty return the empty list
firstN (x : xs) y | y > 0 = x : firstN xs (y - 1) -- if n is greater than 0 add the head to the firstN list and call the function again with the tail and n - 1
                  | otherwise = [] -- n is less than 0 return the firstN list

-- 4. busFinder
busFinder :: Eq t => t -> [(a, [t])] -> [a]
busFinder x [] = [] -- base case, exhausted all routes so now return the list of bus routes that have the bus stop
busFinder x ((y,ys):zs) | (exists x ys) == True = y : busFinder x zs -- will call exist to see if the bus stop exists in the head bus route if it does add it to exist list and call the function again with the bus stop and the rest of the bus routes (the tail)
                        | otherwise = busFinder x zs -- if it doesn't exist in the head bus route call the function again on the stop name and the rest of the bus routes (the tail)

-- 5. cumulativeSums
cumulativeSums :: Num a => [a] -> [a]
cumulativeSums [] = [] -- if list is empty return empty list
cumulativeSums (x : xs) = let -- creating helper function that takes a list and holds the accumulated sum
                          cumulativeSumsHelper [] y = [] -- base case, if empty list return with list of accumulated sum
                          cumulativeSumsHelper (x : xs) y = (y + x) : cumulativeSumsHelper xs (x + y) -- using [1,2,3] with y intially 0 will 0 + 1 inserts 1 into list calls function again with the tail and 1 , 1 + 2 inserts 3 into list and calls function again with tail and 3, 3 + 3 = 6 inserts 6 and calls again reaches base case and exits with the list of accumulated sums
                          in cumulativeSumsHelper (x : xs) 0 -- calling helper function with the list and intial sum = 0

-- 6. groupNleft
groupNleft :: Int -> [a] -> [[a]] 
groupNleft x [] = [] -- if empty list return empty list
groupNleft x (y : ys) = let -- creating helper function thats takes the group size and the input list and the list that holds the group (group list)
                        groupNleftHelper x [] [] = [] -- if input list and group list is both empty return the grouped list (the list with all the groups)
                        groupNleftHelper x [] z = reverse (z) : groupNleftHelper x [] [] -- if input list is empty and the group list is not full than simply add it to the grouped list and call the function with the empty group list to obtain the grouped list
                        groupNleftHelper x (y : ys) z | length z < x = groupNleftHelper x ys (y : z) -- if the group list is not filled up to n than add the value into the group list
                                            | otherwise = reverse (z) : groupNleftHelper x (y : ys) [] -- else add the group into the grouped list
                        in groupNleftHelper x (y : ys) [] -- calling helper function with n the input list and an empty list to hold the group
                   
