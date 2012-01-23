module PH.ShellMain where
{- by Adrien Dudouit-Exposito -}

import Data.List (transpose,replicate)

import PH.Data (Token(..), Table, Col)

import qualified PH.AI   as AI
import qualified PH.Data as Data
import qualified PH.Game as PH
import qualified PH.Rules as Rules
import Data.Maybe
--   -_-'  --
main :: IO ()
main = do
	putStrLn "Welcome in PuissanceHask a '4 in a Row' implementation in Haskell"
	rules
	getRep  (Data.newTable)

-- Internals functions
rules = do
	putStrLn "4-in-a-row is a classic turn-based board game.\n\
	\Drop pieces in the playfield to get 4 lined up in a row, \n\
	\either horizontally, vertically or diagonally."

getInt :: IO Int
getInt = readLn

getRep table = do
	printTable table
	putStrLn "Choose a Collum number where to put your Token"
	rep <- getInt
	if rep > 0 && rep <= Data.tableSize 
		then gameSelect  table rep 
		else do
			putStrLn "bad number"
			getRep table

gameSelect table nbr = do
	if Rules.isFreeCol table nbr then
		haveWin (PH.placeToken table nbr XToken) XToken
	else do
		(putStrLn "column full")
		(printTable table)
		(getRep table)

haveWin table tok 
	|tok == XToken &&(Rules.isWinned table) =do
		putStrLn "congrats You have won !"
		printTable table
		newOrQuit
	|tok == OToken &&(Rules.isWinned table) =do 
		putStrLn "You loose !"
		printTable table
		newOrQuit
	|(Rules.isFullTable table)=do 
		putStrLn "No winner this time"
		printTable table
		newOrQuit
	|tok == OToken = getRep table
	|tok == XToken = haveWin (PH.placeToken table (fromJust(AI.choosePlace table OToken)) OToken) OToken
	
newOrQuit = do
	putStrLn "A next one ? [Y/N]"
	rep <- getLine
	if rep == "Y" || rep =="y"
		then getRep  (Data.newTable)
	else
		if rep == "N" || rep == "n"
			then putStrLn "Goodbye"
		else do
			putStrLn "Bad entry"
			newOrQuit
	
printTable :: Table -> IO()
printTable = putStr . showTable

showTable :: Table -> String
showTable = showTable' . transpose

showTable' :: Table -> String
showTable' (l:ls) = "|"++(showLine l)++(showTable' ls)
showTable' _      = "|"++(replicate (4*Data.tableSize -2) '-')++"-|\n"


showLine :: Col -> String
showLine (v:vs) = " " ++ (show v) ++ " |" ++ (showLine vs)
showLine _      = "\n"