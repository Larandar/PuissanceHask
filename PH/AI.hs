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
-- choosing function
-- first element collum, second the value
chooseBest :: [(Maybe Int,Int)] -> Maybe Int
chooseBest [] = Nothing
chooseBest ls = (\(a,_) -> a) . head . (sortBy (\(_,x) (_,y) -> compare y x)) $ ls



-- fillAllCollum : place in the first free collum
fillAllCollum :: Table -> Token -> Maybe Int
fillAllCollum ((UToken:_):_) _ = Just 1
fillAllCollum (_:table)      t = fillAllCollum table t
fillAllCollum [] _ = Nothing




