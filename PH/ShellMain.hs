module PH.ShellMain where
{- by Adrien Dudouit-Exposito -}

import Data.List (transpose,replicate)

import PH.Data (Token(..), Table, Col)

import qualified PH.Data as Data
import qualified PH.Game as PH

--   -_-'  --
main :: IO ()
main = do
	print "Welcome in PuissanceHask a '4 in a Row' implementation in Haskell"

-- Internals functions
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

