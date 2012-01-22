module PH.AI ( choosePlace ) where
{- by Adrien Dudouit-Exposito -}

import Data.Maybe
import Data.List ( sortBy )

import PH.Data (Token(..), Table, Col, tableSize)
import PH.Game ( placeToken )

import PH.Rules


-- This function is a rename of the desired AI Style
choosePlace :: Table -> Token -> Maybe Int
choosePlace = fillAllCollum

-- AI Type
-- - fillAllCollum : place in the first free collum
-- - fillLineFirst : fill in line the table
-- - minMaxChoose : A min max algorithm that choose the estimate good place


-- choosing function
-- first element collum, second the value
chooseBest :: [(Maybe Int,Int)] -> Maybe Int
chooseBest [] = Nothing
chooseBest ls = (\(a,_) -> a) . head . (sortBy (\(_,x) (_,y) -> compare y x)) $ ls



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

fillAllLine' :: Table -> Token -> Int -> [(Maybe Int,Int)]
fillAllLine'  []    _ _ = []
fillAllLine' (c:cs) t n = (Just n,countUToken c):(fillAllLine' cs t (n+1))

countUToken :: Col -> Int
countUToken  []    = 0
countUToken (UToken:cs) = 1 + countUToken cs
countUToken (_:cs) = countUToken cs

