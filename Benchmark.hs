{- Benchmark file for testing PuissanceHask -}

import Data.Maybe

import PH.Data

import qualified PH.Game
import qualified PH.AI
import qualified PH.Rules
import qualified PH.ShellMain as PH

stack = [(2,OToken),(1,XToken),(6,OToken),(3,XToken),(2,OToken),(3,XToken),(5,OToken),(2,XToken),(3,OToken),(2,XToken)]
table = PH.Game.multiplePlace newTable stack

