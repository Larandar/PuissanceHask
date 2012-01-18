{-       PuissanceHask        -}
{- by Adrien Dudouit-Exposito -}
{- A "4 in a row" in Haskell. -}


-- All module are named PH.***
-- Main modules :
--  - ShellMain for a Shell game mode

import qualified PH.ShellMain as PH.Main

main :: IO ()
main = do 
	PH.Main.main
