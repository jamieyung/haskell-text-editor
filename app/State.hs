{-# LANGUAGE NamedFieldPuns #-}

module State where

import Prelude hiding (lines)

initialState :: State
initialState =
  State
    { above = ["foo"],
      cur = Line {before = "abc", after = "defg"},
      below = ["bar"],
      mode = NormalMode
    }

data State = State
  { above :: [String], -- preceding lines REVERSED
    cur :: Line,
    below :: [String],
    mode :: Mode
  }

data Line = Line
  { before :: String, -- preceeding chars REVERSED!
    after :: String
  }

instance Semigroup Line where
  (Line {before, after})
    <> (Line {before = before', after = after'}) =
      Line {before = reverse before <> after, after = reverse before' <> after'}

instance Show Line where
  show (Line {before, after}) = reverse before <> after

data Mode
  = NormalMode
  | InsertMode
  | CommandMode String
