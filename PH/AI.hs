module PH.AI where
	--( choosePlace ) 
{- by Adrien Dudouit-Exposito -}

import Data.Maybe
import Data.List ( maximumBy, minimumBy )

import PH.Data (Token(..), Table, Col, tableSize)
import PH.Game ( placeToken )

import PH.Rules


-- This function is a rename of the desired AI Style
choosePlace :: Table -> Token -> Maybe Int
choosePlace = minMaxChoose 2

-- AI Type
-- - fillAllCollum : place in the first free collum
-- - fillLineFirst : fill in line the table
-- - minMaxChoose : A min max algorithm that choose the estimate good place


-- choosing function
-- first element collum, second the value
chooseBest :: [(Maybe Int,Int)] -> Maybe Int
chooseBest [] = Nothing
chooseBest ls = (\(a,_) -> a) . bestCoin $ ls

bestCoin :: Ord b => [(a,b)] -> (a,b)
bestCoin  = maximumBy (\(_,x) (_,y) -> compare x y)

worstCoin :: Ord b => [(a,b)] -> (a,b)
worstCoin = minimumBy (\(_,x) (_,y) -> compare x y)

-- minMaxChoose : A min max algorithm that choose the estimate good place
-- - Performance :: Difficulty [(1,< 1 second),(2,+- 3s),(3,17s),(4,2m03s)]
minMaxChoose :: Int -> Table -> Token -> Maybe Int
minMaxChoose difficulty table token 
	| isFullTable table = Nothing
	| otherwise = chooseBest $ map ( computeMMTree token ) ( constructMMTree difficulty table token )

data MMTree = MMNode (Int, Token) Int [MMTree] | MMLeaf (Int, Token) Int deriving (Show)

constructMMTree :: Int -> Table -> Token -> [MMTree]
constructMMTree diff table token = map (nextMMTree diff table token') (map (\x -> (x,token)) [1..tableSize])
	where token' = if token == XToken
		then OToken
		else XToken

nextMMTree :: Int -> Table -> Token -> (Int,Token) -> MMTree
nextMMTree diff table token (p,t)
	| not (isFreeCol table p) = MMLeaf (p,t) (-100)
	| isJust win && win /= (Just token) = MMLeaf (p,t) looVal
	| isJust win && win == (Just token) = MMLeaf (p,t) winVal
	| diff == 0                    = MMLeaf (p,t) blankVal
	| isNothing win                = MMNode (p,t) blankVal ( constructMMTree (diff-1) next token )
	where
		next = placeToken table p t'
		win = gameWinner next
		winVal = 100
		looVal = (-100)
		blankVal = (-10)
		t' =  if t == XToken
			then OToken
			else XToken

sameToken :: Maybe Token -> Maybe Token -> Bool
sameToken (Just a) (Just b) = a == b
sameToken _ _ = False

computeMMTree :: Token -> MMTree -> (Maybe Int,Int)
computeMMTree token (MMNode (a, t) v c)
	 | token == t = (\(_,x) -> (Just a, v+x)) . bestCoin $ map ( computeMMTree token ) c
	 | otherwise  = (\(_,x) -> (Just a, v+x)) . worstCoin $ map ( computeMMTree token ) c
computeMMTree _ (MMLeaf (a, _) v) = (Just a, v)



-- fillAllCollum : place in the first free collum
fillAllCollum :: Table -> Token -> Maybe Int
fillAllCollum ((UToken:_):_) _ = Just 1
fillAllCollum (_:table)      t = justInc (fillAllCollum table t)
fillAllCollum []             _ = Nothing

justInc :: Maybe Int -> Maybe Int
justInc (Just a) = Just (a+1)
justInc Nothing  = Nothing

-- fillLineFirst : fill in line the table
fillAllLine :: Table -> Token -> Maybe Int
fillAllLine table t = chooseBest $ fillAllLine' table t 1

fillAllLine' :: Table -> Token -> Int -> [(Maybe Int,Int)]
fillAllLine'  []    _ _ = []
fillAllLine' (c:cs) t n = (Just n,countUToken c):(fillAllLine' cs t (n+1))

countUToken :: Col -> Int
countUToken  []    = 0
countUToken (UToken:cs) = 1 + countUToken cs
countUToken (_:cs) = countUToken cs

