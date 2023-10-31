module Bowling (score, BowlingError(..)) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

data GameState = GameState { frame :: Int
                           , points :: Int
                           , firstRoll :: Bool
                           , afterSpare :: Bool
                           , firstAfterStrike :: Bool
                           , secondAfterStrike :: Bool
                           }

newGame :: GameState
newGame = GameState { frame = 1
                    , points = 0
                    , firstRoll = True
                    , afterSpare = False
                    , firstAfterStrike = False
                    , secondAfterStrike = False
                    }

score :: [Int] -> Either BowlingError Int
score rolls = evalStateT bar newGame

bar :: StateT GameState (Either BowlingError) Int
bar = do
  lift $ Right 42
