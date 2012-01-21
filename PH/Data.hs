module PH.Data ( 
	Token(..), Table, Col, PileExec,
	tableSize, newTable,
	) where
{- by Adrien Dudouit-Exposito -}

import Data.Maybe

data Token = UToken | OToken | XToken deriving ( Eq, Enum )
instance  Show Token  where
	show UToken = " "
	show OToken = "O"
	show XToken = "X"

type Table = [  Col  ]
type Col   = [ Token ]

type PileExec = [(Int, Token)]

tableSize = 8

newTable :: Table
newTable = newTable' tableSize

-- Internals functions

newTable' :: Int -> Table
newTable' 0 = []
newTable' n = (newCol tableSize):(newTable' $ n-1)

newCol :: Int -> Col
newCol 0 = []
newCol n = (UToken):(newCol $ n-1)
