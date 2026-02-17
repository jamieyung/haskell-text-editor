{-# LANGUAGE OverloadedRecordDot #-}
module Update where

import Data.List (unsnoc)
import Graphics.Vty hiding (Mode)
import Prelude hiding (lines)
import State
import Util

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
    EvKey (KChar 'a') [] -> NextState $ st {mode = InsertMode, cx = st.cx + 1}
    EvKey (KChar 'i') [] -> NextState $ st {mode = InsertMode}
    EvKey (KChar 'o') [] -> do
      let prevLines = take (st.cy + 1) st.lines
          postLines = drop (st.cy + 1) st.lines
          newLines = prevLines ++ [""] ++ postLines
      NextState $ st {mode = InsertMode, lines = newLines, cx = 0, cy = st.cy + 1}
    -- entering command mode
    EvKey (KChar ':') [] -> NextState $ st {mode = CommandMode ""}
    -- movement
    EvKey (KChar 'h') [] -> NextState $ moveCursorLeft st
    EvKey (KChar 'j') [] -> NextState $ moveCursorVert 1 st
    EvKey (KChar 'k') [] -> NextState $ moveCursorVert (-1) st
    EvKey (KChar 'l') [] -> NextState $ moveCursorRight st
    EvKey (KChar 'w') [] -> NextState $ moveCursorForwardWord st
    EvKey (KChar 'b') [] -> NextState $ moveCursorBackwardWord st
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
    EvKey KBS [] | st.cx == 0 && st.cy > 0 -> do
      -- put cur line at end of prev line
      let curLine = st.lines !! st.cy
          prevLine = st.lines !! (st.cy - 1)
          pre = take (st.cy - 1) st.lines
          post = drop (st.cy + 1) st.lines
          newLines = pre ++ [prevLine <> curLine] ++ post
      NextState $ st {lines = newLines, cx = length prevLine, cy = st.cy - 1}
    EvKey KBS [] | st.cx > 0 -> do
      -- delete char from cur line
      let curLine = st.lines !! st.cy
          pre = take (st.cx - 1) curLine
          post = drop st.cx curLine
          newLine = pre ++ post
          newLines = modifyAt st.cy (const newLine) st.lines
      NextState $ st {lines = newLines, cx = st.cx - 1}
    EvKey (KChar c) [] -> do
      -- insert char at cursor
      let curLine = st.lines !! st.cy
          pre = take st.cx curLine
          post = drop st.cx curLine
          newLine = pre ++ [c] ++ post
          newLines = modifyAt st.cy (const newLine) st.lines
      NextState $ st {lines = newLines, cx = st.cx + 1}
    EvKey KEnter [] -> do
      -- insert newline (TODO: automatic indentation)
      let curLine = st.lines !! st.cy
          pre = take st.cx curLine
          post = drop st.cx curLine
          prevLines = take st.cy st.lines
          postLines = drop (st.cy + 1) st.lines
          newLines = prevLines ++ [pre, post] ++ postLines
      NextState $ st {lines = newLines, cx = 0, cy = st.cy + 1}
    EvKey KEsc [] ->
      let
       in NextState $ ensureCXisInCurLineBounds $ st {mode = NormalMode}
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

moveCursorLeft :: State -> State
moveCursorLeft st = ensureCXisInCurLineBounds $ st {cx = st.cx - 1}

moveCursorRight :: State -> State
moveCursorRight st = ensureCXisInCurLineBounds $ st {cx = st.cx + 1}

moveCursorForwardWord :: State -> State
moveCursorForwardWord st = st

moveCursorBackwardWord :: State -> State
moveCursorBackwardWord st = st

ensureCXisInCurLineBounds :: State -> State
ensureCXisInCurLineBounds st =
  let curLine = st.lines !! st.cy
   in st {cx = max 0 $ min (length curLine - 1) st.cx}

moveCursorVert :: Int -> State -> State
moveCursorVert dy st =
  let newY = max 0 $ min (length st.lines - 1) (st.cy + dy)
      newX =
        if newY == st.cy
          then st.cx
          else
            let curLine = st.lines !! newY
             in min (length curLine - 1) st.cx
   in st {cx = newX, cy = newY}
