{-# LANGUAGE RecordWildCards #-}

module Bowling (score, BowlingError(..)) where

import Control.Monad (replicateM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)
import Prelude hiding (error)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

data Frame = OneBallThrown Int | OneBonusBall Int | TwoBonusBalls Int | Complete Int
  deriving (Show)

data GameState = GameState { rolls :: [Int]
                           , current_roll :: Int
                           , max_roll :: Int
                           } deriving Show

type Evaluator = StateT GameState (Either BowlingError)

newGame :: [Int] -> GameState
newGame rolls = GameState { rolls = rolls
                          , current_roll = 0
                          , max_roll = 0
                          }

score :: [Int] -> Either BowlingError Int
score rolls =  evalStateT calculate (newGame rolls)

calculate :: Evaluator Int
calculate = do
  scores <- replicateM 10 frame
  return $ sum scores

frame :: Evaluator Int
frame = do
  roll1 <- getRoll
  case roll1 of
    10 -> do
      roll2 <- peekRoll 0
      roll3 <- peekRoll 1
      return $ 10 + roll2 + roll3
    _ -> do
      roll2 <- getRoll
      case roll1 + roll2 of
        10 -> do
          roll3 <- peekRoll 0
          return $ 10 + roll3
        total -> do
          return total

getRoll :: Evaluator Int
getRoll = do
  roll <- peekRoll 0
  state@GameState{..} <- get
  put state { current_roll = current_roll + 1 }
  return roll

peekRoll :: Int -> Evaluator Int
peekRoll offset = do
  state@GameState{..} <- get
  let index = current_roll + offset
  when (index >= length rolls) $ lift $ Left IncompleteGame
  let roll = rolls !! index
  when (roll < 0 || roll > 10) $ lift $ Left $ InvalidRoll index roll
  put state { max_roll = max index max_roll }
  return roll
