module PH.Game ( placeToken, multiplePlace ) where
{- by Adrien Dudouit-Exposito -}

import Data.Maybe

import PH.Data

import qualified PH.Rules as Rules

placeToken :: Table -> Int -> Token -> Table
placeToken table col token = setToken table col lig token
	where lig = fromJust ( Rules.wherePlace table col )

multiplePlace :: Table -> PileExec -> Table
multiplePlace table []               = table
multiplePlace table ((col,token):ls) = placeToken (multiplePlace table ls) col token


-- Internal function
setToken :: Table -> Int -> Int -> Token -> Table
setToken [] _ _ _  = error "Not enough big table"
setToken table col lig token 
	| col == 1     = ( setColToken (head table) lig token ):( tail table )
	| col  > 1     = ( head table ):( setToken ( tail table ) ( col - 1 ) lig token )
	| otherwise    = error $ "Not valable call ("++(show col)++","++(show lig)++") in "++(show table)

setColToken :: Col -> Int -> Token -> Col
setColToken [] _ _ = error "Not enough big collum"
setColToken col lig token
	| lig == 1     = token:(tail col)
	| lig  > 1     = (head col):(setColToken (tail col) ( lig - 1 ) token )
	| otherwise    = error $ "Not valable call ("++(show lig)++") in "++(show col)

