{-# LANGUAGE RecordWildCards #-}

module Bowling (score, BowlingError(..)) where

import Control.Applicative ((<|>))
import Control.Monad (guard, replicateM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)

data BowlingError = IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

data Frame = OneBallThrown Int | OneBonusBall Int | TwoBonusBalls Int | Complete Int
  deriving (Show)

data GameState = GameState { rolls :: [Int]
                           , current_roll :: Int
                           , max_roll :: Int
                           , error :: BowlingError 
                           }

type Evaluator = StateT GameState (Either BowlingError)

newGame :: [Int] -> GameState
newGame rolls = GameState { rolls = rolls
                          , current_roll = 0
                          , max_roll = 0
                          , error = IncompleteGame 
                          }

score :: [Int] -> Either BowlingError Int
score rolls =  evalStateT calculate (newGame rolls)

calculate :: Evaluator Int
calculate = do
  scores <- replicateM 10 frame
  return $ sum scores

frame :: Evaluator Int
frame = do
  roll <- getRoll
  case roll of
    10 -> do
      x <- getRoll
      y <- getRoll
      rollBack
      rollBack
      return $ 10 + x + y
    _ -> do
      x <- getRoll
      case roll + x of
        10 -> do
          y <- getRoll
          rollBack
          return $ 10 + y
        _ -> do
          return $ roll + x

rollBack :: Evaluator ()
rollBack = do
  state@GameState{..} <- get
  put state { current_roll = current_roll - 1 }

getRoll :: Evaluator Int
getRoll = do
  state@GameState{..} <- get
  when (current_roll >= length rolls) $ lift $ Left IncompleteGame
  let roll = rolls !! current_roll
  when (roll < 0 || roll > 10) $ lift $ Left $ InvalidRoll current_roll roll
  put state { current_roll = current_roll + 1, max_roll = max current_roll max_roll }
  return $ rolls !! current_roll
