{-# LANGUAGE RecordWildCards #-}

module Bowling (score, BowlingError(..)) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

data GameState = GameState { rollsLeft :: [Int]
                           , frame :: Int
                           , points :: Int
                           , firstRoll :: Bool
                           , firstRollScore :: Int
                           , afterSpare :: Bool
                           , firstAfterStrike :: Bool
                           , secondAfterStrike :: Bool
                           }

newGame :: [Int] -> GameState
newGame rolls = GameState { rollsLeft = rolls
                          , frame = 1
                          , points = 0
                          , firstRoll = True
                          , firstRollScore = 0
                          , afterSpare = False
                          , firstAfterStrike = False
                          , secondAfterStrike = False
                          }

score :: [Int] -> Either BowlingError Int
score rolls = evalStateT bar (newGame rolls)

bar :: StateT GameState (Either BowlingError) Int
bar = do
  state@GameState{..} <- get
  case rollsLeft of
    [] -> lift $ Right points
    (x:xs) -> do
      put (state { rollsLeft = xs
                 , points = points + if afterSpare then 2*x else x
                 , firstRoll = not firstRoll
                 , firstRollScore = if firstRoll then x else 0
                 , afterSpare = firstRollScore + x == 10 })
      bar
