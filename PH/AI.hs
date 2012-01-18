module PH.AI ( choosePlace ) where
{- by Adrien Dudouit-Exposito -}

import PH.Data (Token(..), Table, Col)

-- This function is a rename of the desired AI Style
choosePlace :: Table -> Token -> Maybe Int
choosePlace = fillAllCollum

-- AI Type 

-- Random
randomOne :: [Int] -> Int
randomOne = head


-- fillAllCollum : place in the first free collum
fillAllCollum :: Table -> Token -> Maybe Int
fillAllCollum ((UToken:_):_) _ = Just 1
fillAllCollum (_:table)      t = fillAllCollum table t
fillAllCollum [] _ = Nothing



-- choosing function
-- TODO Implement the choosing function in a list of value with weight (bigger is the weight bette is the play) if all values are the same choose a random of there
chooseBest :: [(Int,Float)] -> Int
chooseBest _ = 1

