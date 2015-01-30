--Domashna 9 
--Dimo Drangov 80936

import Data.Char

--Zadacha 1
merge :: [a] -> [a] -> [a]
merge [] l2 = l2
merge l1 [] = l1
merge l1 l2 = concat [[x,y] | (x,y) <- zip l1 l2]
	
--Zadacha 2
hammingDistance :: String -> String -> Int
hammingDistance str1 str2 
	|str1 == [] = 0
	|head str1 == head str2 = hammingDistance (tail str1) (tail str2)
	|otherwise = 1 + hammingDistance (tail str1) (tail str2)
	
--Zadacha 3
lowercase :: [Char] -> [Char]
lowercase str = map toLower str
	
occuarances :: String -> [String] -> Int
occuarances str1 str2
	|str2 == [] = 0
	|lowercase str1 == lowercase (head str2) = 1 + occuarances str1 (tail str2)
	|otherwise = occuarances str1 (tail str2)

isInList :: String -> [String] -> Bool
isInList str arr
	|arr == [] = False
	|lowercase (head arr) == lowercase str = True
	|otherwise = isInList str (tail arr)
	
withoutRepHelper :: [String] -> [String] -> [String]
withoutRepHelper str1 str2
	|str1 == [] = str2
	|isInList (head str1) str2 = withoutRepHelper (tail str1) str2
	|otherwise = withoutRepHelper (tail str1) (str2 ++ [lowercase (head str1)])
	
withoutRep :: [String] -> [String]
withoutRep str = withoutRepHelper str []

dictionaryHelper :: String -> [String] -> [(String, Int)]
dictionaryHelper str1 str2
	|str2 == [] = []
	|otherwise = [((head str2),(occuarances (head str2) (words str1)))] ++ dictionaryHelper str1 (tail str2)
	
dictionary :: String -> [(String, Int)]
dictionary str = dictionaryHelper str (withoutRep (words str))

--Zadacha 4