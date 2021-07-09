-- CptS 355 - Sample Functions

module SampleFunctions
     where

import Data.Char

--isNull
isNull :: [a] -> Bool
isNull [] = True
isNull (x:xs) = False

-- length of a list
length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + (length' xs)

-- last'
last' :: [a] -> a
last' []     = error "last': Input list is empty."
last' [x]    = x 
last' (x:xs) = (last' xs)


-- nth element
nthElement [] n = error "nthElement': The input list is too short."
nthElement (x:xs) 1 = x
nthElement (x:xs) n = (nthElement xs (n-1))


-- allSquares
allSquares :: Num a => [a] -> [a]
allSquares [] = []
allSquares (x : xs) = x * x : allSquares xs
 

-- strToUpperq
strToUpper :: String -> String
strToUpper [] = []
strToUpper (chr : xs) = (toUpper chr) : (strToUpper xs)


-- filter if smaller than v
filterSmaller [] v = [] 
filterSmaller (x:xs) v | (x >= v) = x:(filterSmaller xs v)
                       | otherwise = (filterSmaller xs v)

--extractDigits
extractDigits :: String -> String
extractDigits [] = []
extractDigits (chr : xs) | isDigit chr = chr : extractDigits xs
                         | otherwise = extractDigits xs


myChar = nthElement "CptS355-Assignment1" 5

main = do print ( nthElement [1,2,3,4,5] 4)
          print ( allSquares [1,2,3,4,5] )
          print ( length [1,2,3,4,5] )
          print (myChar)