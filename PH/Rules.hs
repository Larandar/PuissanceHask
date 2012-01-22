module PH.Rules ( wherePlace, gameWinner, isWinned, isFullTable, isFreeCol ) where
{- by Adrien Dudouit-Exposito -}

import Data.Maybe
import Data.List (transpose)

import PH.Data

wherePlace :: Table -> Int -> (Maybe Int)
wherePlace table c = wherePlaceCol (table!!(c-1))

gameWinner :: Table -> Maybe Token
gameWinner table = gameWinnerExplorer (transpose table)

isWinned :: Table -> Bool
isWinned = isJust . gameWinner

isFullTable :: Table -> Bool
isFullTable []             = True
isFullTable ((UToken:_):_) = False
isFullTable (_:cs)         = isFullTable cs

isFreeCol :: Table -> Int -> Bool
isFreeCol ((UToken:_):_) 1 = True
isFreeCol _              1 = False
isFreeCol (c:cs)         n = isFreeCol cs (n-1)

-- Internal funtions
-- Used by wherePlace
wherePlaceCol :: Col -> (Maybe Int)
wherePlaceCol (UToken:col) = justAdd (wherePlaceCol col) 1
wherePlaceCol _            = Nothing

justAdd :: Maybe Int -> Int -> Maybe Int
justAdd (Just a) b = Just (a+b)
justAdd Nothing  b = Just   b  

-- Used for winning detection
gameWinnerExplorer :: Table -> Maybe Token
gameWinnerExplorer [] = Nothing
gameWinnerExplorer (l:ls)
	| isJust lineEx   = lineEx
	| otherwise       = gameWinnerExplorer ls
	where lineEx = lineWinnerExploration ([],l,ls,1)

lineWinnerExploration :: ([Line],Line,[Line],Int) -> Maybe Token
lineWinnerExploration pos@(b,l,n,p)
	| n == [] && p == tableSize = Nothing
	| not vid && or winning     = Just act
	| p == tableSize = lineWinnerExploration (l:b,head n,tail n,1)
	| otherwise      = lineWinnerExploration (b,l,n,p+1)
	where
		act = l!!(p-1)
		vid = act == UToken
		winning = map (>= 4) [numberOfDir dirEE pos act, numberOfDir dirSE pos act, numberOfDir dirSS pos act, numberOfDir dirSW pos act]

numberOfDir :: Dir -> ([Line],Line,[Line],Int) -> Token -> Int
numberOfDir dir pos@(_,l,_,p) token
	| act /= token   = 0
	| isNothing next = 1
	| isJust next    = 1 + numberOfDir dir (fromJust next) token
	where
		act  = l!!(p-1)
		next = dir pos

type Dir = ([Line],Line,[Line],Int) -> Maybe ([Line],Line,[Line],Int)

dirNE :: Dir
dirNE (b,l,n,p)
	| p >= tableSize || b == [] = Nothing
	| otherwise = Just (tail b,head b,l:n,p+1)

dirEE :: Dir
dirEE (b,l,n,p)
	| p >= tableSize = Nothing
	| otherwise = Just (b,l,n,p+1)

dirSE :: Dir
dirSE (b,l,n,p)
	| p >= tableSize || n == [] = Nothing
	| otherwise = Just (l:b,head n,tail n,p+1)

dirSS :: Dir
dirSS (b,l,n,p)
	| n == []   = Nothing
	| otherwise = Just (l:b,head n,tail n,p)

dirSW :: Dir
dirSW (b,l,n,p)
	| p <= 1 || n == [] = Nothing
	| otherwise = Just (l:b,head n,tail n,p-1)

dirWW :: Dir
dirWW (b,l,n,p)
	| p <= 1    = Nothing
	| otherwise = Just (b,l,n,p-1)

dirNW :: Dir
dirNW (b,l,n,p)
	| p <= 1 || b == []   = Nothing
	| otherwise = Just (tail b,head b,l:n,p-1)



