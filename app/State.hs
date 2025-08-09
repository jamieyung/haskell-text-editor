{-# LANGUAGE NamedFieldPuns #-}

module State where

import Prelude hiding (lines)

initialState :: State
initialState =
  State
    { above = [Line {before = "foo", after = ""}],
      cur = Line {before = "abc", after = "defg"},
      below = [],
      mode = NormalMode
    }

data State = State
  { above :: [Line],
    cur :: Line,
    below :: [Line],
    mode :: Mode
  }

data Line = Line {before :: String, after :: String}

instance Semigroup Line where
  (Line {before, after})
    <> (Line {before = before', after = after'}) =
      Line {before = before <> after, after = before' <> after'}

data Mode
  = NormalMode
  | InsertMode
  | CommandMode String
