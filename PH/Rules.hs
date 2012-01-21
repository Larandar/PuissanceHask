module PH.Rules ( wherePlace, gameWinner, isFullTable ) where
{- by Adrien Dudouit-Exposito -}

import Data.Maybe

import PH.Data

wherePlace :: Table -> Int -> (Maybe Int)
wherePlace table c = wherePlaceCol (table!!(c-1))

isFullTable :: Table -> Bool
isFullTable []             = True
isFullTable ((UToken:_):_) = False
isFullTable (_:cs)         = isFullTable cs

-- TODO Implement this winning condition function
gameWinner :: Table -> Maybe Token
gameWinner _ = Nothing

-- Internal funtions
wherePlaceCol :: Col -> (Maybe Int)
wherePlaceCol (UToken:col) = justAdd (wherePlaceCol col) 1
wherePlaceCol _            = Nothing

justAdd :: Maybe Int -> Int -> Maybe Int
justAdd (Just a) b = Just (a+b)
justAdd Nothing  b = Just   b  
