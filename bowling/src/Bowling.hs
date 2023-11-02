{-# LANGUAGE RecordWildCards #-}

module Bowling (score, BowlingError(..)) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

data Frame = OneBallThrown Int | OneBonusBall Int | TwoBonusBalls Int | Complete Int
  deriving (Show)

data GameState = GameState { frames :: [Frame], rolls :: [Int] }

newGame :: [Int] -> GameState
newGame rolls = GameState { frames = [], rolls = rolls }

score :: [Int] -> Either BowlingError Int
score rolls = evalStateT calculate (newGame rolls)

calculate :: StateT GameState (Either BowlingError) Int
calculate = do
  GameState{..} <- get
  return $ sum rolls
