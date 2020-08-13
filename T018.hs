import DataFile

-- Q1
wordToken:: String -> [String]
wordToken x = wordTokenHelper (words x)

wordTokenHelper [] = []
wordTokenHelper (x:xs) = if isElement x punct then (take ((length x) - 1) x) : [last x] : wordTokenHelper xs
					      else x:wordTokenHelper xs
isElement [] _ = False
isElement (x:xs) l = if elem x l then True else isElement xs l

-- Q2
wordTokenList :: [String] -> [String]
wordTokenList [] =  []
wordTokenList (x:xs) = (wordToken x) ++ wordTokenList xs

-- Q3
removeDuplicates [] = []
removeDuplicates (x:xs) = if elem x xs then removeDuplicates xs
				       else x:removeDuplicates xs

uniqueBigrams :: [String] -> [(String,String)]
uniqueBigrams x = removeDuplicates (uniqueBigramsHelper x)

uniqueBigramsHelper [] = []
uniqueBigramsHelper [_] = []
uniqueBigramsHelper (x:y:xs) = (x,y):uniqueBigramsHelper (y:xs)

-- Q4
uniqueTrigrams :: [String] -> [(String,String,String)]
uniqueTrigrams x = removeDuplicates (uniqueTrigramsHelper x)

uniqueTrigramsHelper [] = []
uniqueTrigramsHelper [_] = []
uniqueTrigramsHelper [_,_] = []
uniqueTrigramsHelper (x:y:z:xs) = (x,y,z) : uniqueTrigramsHelper (y:z:xs)

-- Q5
bigramsFreq :: Num a => [String] -> [((String,String),a)]
bigramsFreq x = placeFreq (uniqueBigrams x) (uniqueBigramsHelper x)

placeFreq [] _ = []
placeFreq (x:xs) y = (x,getCount x y):placeFreq xs y

getCount _ [] = 0
getCount x (y:ys) = if (x == y) then 1 + getCount x ys
				else getCount x ys

-- Q6
trigramsFreq:: Num a => [String] -> [((String,String,String),a)]
trigramsFreq x = placeFreq (uniqueTrigrams x) (uniqueTrigramsHelper x)

-- Q7
getFreq :: (Eq a, Num b) => a -> [(a,b)] -> b
getFreq _ [] = 0
getFreq x ((y1,y2):ys) = if (x == y1) then y2
				      else getFreq x ys

-- Q8
generateOneProb :: Fractional a => ((String,String,String),a) -> [((String,String),a)] -> a
generateOneProb ((x,y,z),fr1) l = fr1 / (getFreq (x,y) l)

-- Q9
--genProbPairs :: Fractional a => [((String,String,String),a)] -> [((String,String),a)] -> [((String,String,String),a)]
genProbPairs [] _ = []
genProbPairs (((x,y,z),a):xs) l = ((x,y,z),generateOneProb ((x,y,z),a) l):genProbPairs xs l

-- Q10
generateNextWord :: (Ord a, Fractional a) => [String] -> [((String,String,String),a)] -> String
generateNextWord (x1:x2:xs) l = if((length (getOptions (x1,x2) l)) == 0) then (error "Sorry, it is not possible to infer from current database")
									 else (getOptions (x1,x2) l) !! (randomZeroToX ((length (getOptions (x1,x2) l)) - 1))

getOptions _ [] = []
getOptions (x1,x2) (((y1,y2,y3),yFreq):ys) = if ((x1 == y1) && (x2 == y2) && (yFreq > 0.03)) then y3 : getOptions (x1,x2) ys
									  else getOptions (x1,x2) ys


-- Q11
generateText :: String -> Int -> String
generateText s 0 = s
generateText s n = if (elem (x!!0) punct) then generateText (s ++ x) (n-1) else generateText ((s ++ " ") ++ x) (n-1)
		   where x = generateNextWord (wordToken ((last ((init (words s))) ++ " ") ++ (last (words s)))) (genProbPairs (trigramsFreq (wordTokenList (docs))) (bigramsFreq (wordTokenList (docs))))

-- Evaluation
newPunct =  ['.','!','?']
sentToken :: String -> [String]
sentToken s = removeHelper (sentTokenH s "")

sentTokenH [] [] = [[]]
sentTokenH (x:xs) y = if elem x newPunct then (y ++ [x]):sentTokenH xs ""
		      else sentTokenH xs (y ++ [x])

removeHelper [_] = []
removeHelper (x:xs) = if (x !! 0) == ' ' then tail x:removeHelper xs else x:removeHelper xs
