{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Update where

import Data.List (unsnoc)
import Graphics.Vty hiding (Mode)
import State
import Prelude hiding (lines)

data UpdateResult = Quit | NextState State

update :: Event -> State -> UpdateResult
update e st = case st.mode of
  NormalMode -> updateNormalMode e st
  InsertMode -> updateInsertMode e st
  CommandMode s -> updateCommandMode e st s

updateNormalMode :: Event -> State -> UpdateResult
updateNormalMode e st =
  case e of
    -- ways of entering insert mode
    EvKey (KChar 'a') [] ->
      NextState $
        st
          { cur = Line {before = take 1 (after . cur $ st) <> (before . cur $ st), after = drop 1 $ after . cur $ st},
            mode = InsertMode
          }
    EvKey (KChar 'i') [] -> NextState $ st {mode = InsertMode}
    EvKey (KChar 'o') [] -> NextState $ (\st' -> st' {mode = InsertMode}) $ insertBlankLineBelow st
    -- entering command mode
    EvKey (KChar ':') [] -> NextState $ st {mode = CommandMode ""}
    -- movement
    EvKey (KChar 'h') [] -> NextState $ moveCursorLeft st
    EvKey (KChar 'j') [] -> NextState $ moveCursorVert 1 st
    EvKey (KChar 'k') [] -> NextState $ moveCursorVert (-1) st
    EvKey (KChar 'l') [] -> NextState $ moveCursorRight st
    EvKey KLeft [] -> NextState $ moveCursorLeft st
    EvKey KDown [] -> NextState $ moveCursorVert 1 st
    EvKey KUp [] -> NextState $ moveCursorVert (-1) st
    EvKey KRight [] -> NextState $ moveCursorRight st
    _ -> NextState st

updateInsertMode :: Event -> State -> UpdateResult
updateInsertMode e st =
  case e of
    EvKey KLeft [] -> NextState $ moveCursorLeft st
    EvKey KDown [] -> NextState $ moveCursorVert 1 st
    EvKey KUp [] -> NextState $ moveCursorVert (-1) st
    EvKey KRight [] -> NextState $ moveCursorRight st
    EvKey KBS [] -> NextState $ backSpaceOne st
    EvKey (KChar c) [] -> NextState $ insertChar c st
    EvKey KEnter [] -> NextState $ insertNewLine st
    EvKey KEsc [] ->
      let
       in NextState $ st {mode = NormalMode}
    _ -> NextState st

updateCommandMode :: Event -> State -> String -> UpdateResult
updateCommandMode e st s =
  case e of
    EvKey (KChar c) [] -> NextState $ st {mode = CommandMode $ s ++ [c]}
    EvKey KBS [] ->
      NextState $
        st
          { mode = case unsnoc s of
              Nothing -> NormalMode
              Just (cs, _) -> CommandMode cs
          }
    EvKey KEnter [] -> case s of
      "q" -> Quit
      _ -> NextState $ st {mode = NormalMode}
    EvKey KEsc [] ->
      let
       in NextState $ st {mode = NormalMode}
    _ -> NextState st

insertBlankLineBelow :: State -> State
insertBlankLineBelow st@State {above, cur} =
  st
    { above = show cur : above,
      cur = Line {before = "", after = ""}
    }

insertChar :: Char -> State -> State
insertChar c st@State {cur = Line {before, after}} =
  st {cur = Line {before = c : before, after}}

insertNewLine :: State -> State
insertNewLine st@State {above, cur = Line {before, after}, below} =
  st
    { above = reverse before : above,
      cur = Line {before = [], after},
      below
    }

backSpaceOne :: State -> State
backSpaceOne st@State {above = [], cur = Line {before = []}} = st
backSpaceOne st@State {above = (x : xs), cur = curLine@Line {before = []}, below} =
  st {above = xs, cur = Line {before = x, after = []} <> curLine, below}
backSpaceOne st@State {cur = Line {before = (_ : xs), after}} =
  st {cur = Line {before = xs, after}}

moveCursorLeft :: State -> State
moveCursorLeft st@State {cur = Line {before = []}} = st
moveCursorLeft st@State {cur = Line {before = (x : xs), after}} =
  st {cur = Line {before = xs, after = x : after}}

moveCursorRight :: State -> State
moveCursorRight st@State {cur = Line {after = []}} = st
moveCursorRight st@State {cur = Line {before, after = (x : xs)}} =
  st {cur = Line {before = x : before, after = xs}}

moveCursorVert :: Int -> State -> State
moveCursorVert _ st@State {above = [], below = []} = st
moveCursorVert dy st
  | dy > 0 = (!! dy) . iterate moveCursorDown $ st
  | dy < 0 = (!! negate dy) . iterate moveCursorUp $ st
  | otherwise = st

moveCursorDown :: State -> State
moveCursorDown st@State {below = []} = st
moveCursorDown st@State {above, cur = curLine@Line {before}, below = (x : xs)} =
  st
    { above = show curLine : above,
      cur = Line {before = reverse $ take (length before) x, after = drop (length before) x},
      below = xs
    }

moveCursorUp :: State -> State
moveCursorUp st@State {above = []} = st
moveCursorUp st@State {above = (x : xs), cur = curLine@Line {before}, below} =
  st
    { above = xs,
      cur = Line {before = reverse $ take (length before) x, after = drop (length before) x},
      below = show curLine : below
    }
