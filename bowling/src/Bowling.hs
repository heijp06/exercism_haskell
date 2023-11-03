{-# LANGUAGE RecordWildCards #-}

module Bowling (score, BowlingError(..)) where

import Control.Applicative ((<|>))
import Control.Monad (guard, replicateM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Lazy (StateT, evalStateT, get, put)

data BowlingError = NoError
                  | IncompleteGame
                  | InvalidRoll { rollIndex :: Int, rollValue :: Int }
  deriving (Eq, Show)

data Frame = OneBallThrown Int | OneBonusBall Int | TwoBonusBalls Int | Complete Int
  deriving (Show)

data GameState = GameState { rolls :: [Int]
                           , current_roll :: Int
                           , max_roll :: Int
                           , error :: BowlingError 
                           }

type Evaluator = StateT GameState Maybe

newGame :: [Int] -> GameState
newGame rolls = GameState { rolls = rolls
                          , current_roll = 0
                          , max_roll = 0
                          , error = IncompleteGame 
                          }

score :: [Int] -> Either BowlingError Int
score rolls = case evalStateT calculate (newGame rolls) of
  Just score -> Right score
  _ -> Left IncompleteGame

calculate :: Evaluator Int
calculate = do
  scores <- replicateM 10 frame
  return $ sum scores

frame :: Evaluator Int
frame = strike <|> spare <|> openFrame

strike :: Evaluator Int
strike = do
  roll <- getRoll
  case roll of
    10 -> do
      x <- getRoll
      y <- getRoll
      rollBack
      rollBack
      return $ roll + x + y
    _ -> do
      rollBack
      lift Nothing

spare :: Evaluator Int
spare = do
  roll <- getRoll
  case roll of
    10 -> do
      rollBack
      lift Nothing
    _ -> do
      x <- getRoll
      case roll + x of
        10 -> do
          y <- getRoll
          rollBack
          return $ roll + x + y
        _ -> do
          rollBack
          rollBack
          lift Nothing

openFrame :: Evaluator Int
openFrame = do
  roll <- getRoll
  case roll of
    10 -> do
      rollBack
      lift Nothing
    _ -> do
      x <- getRoll
      case roll + x of
        10 -> do
          rollBack
          rollBack
          lift Nothing
        _ -> do
          return $ roll + x

rollBack :: Evaluator ()
rollBack = do
  state@GameState{..} <- get
  put state { current_roll = current_roll - 1 }

getRoll :: Evaluator Int
getRoll = do
  state@GameState{..} <- get
  when (current_roll >= length rolls) $ raiseError IncompleteGame
  let roll = rolls !! current_roll
  when (roll < 0 || roll > 10) $ raiseError $ InvalidRoll current_roll roll
  put state { current_roll = current_roll + 1, max_roll = max current_roll max_roll }
  return $ rolls !! current_roll

raiseError :: BowlingError -> Evaluator ()
raiseError err = do
  state@GameState{..} <- get
  put state { error = err }
  lift Nothing
