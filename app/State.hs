module State where

import Prelude hiding (lines)

initialState :: State
initialState =
  State
    { lines = ["hi", "wolrd"],
      cx = 0,
      cy = 0,
      mode = NormalMode
    }

data State = State
  { lines :: [String],
    cx :: Int,
    cy :: Int,
    mode :: Mode
  }

data Mode
  = NormalMode
  | InsertMode
  | CommandMode String
