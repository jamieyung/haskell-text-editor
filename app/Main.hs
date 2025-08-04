{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Data.Function ((&))
import Data.List (unsnoc)
import Graphics.Vty hiding (Mode)
import Graphics.Vty.Platform.Unix (mkVty)
import Prelude hiding (lines)

main :: IO ()
main = do
  vty <- mkVty defaultConfig
  eventLoop vty initialState

eventLoop :: Vty -> State -> IO ()
eventLoop vty st = do
  draw vty st
  e <- nextEvent vty
  case Main.update e st of
    Quit -> shutdown vty
    NextState st' -> eventLoop vty st'

data UpdateResult = Quit | NextState State

update :: Event -> State -> UpdateResult
update e st = case mode st of
    NormalMode ->
      case e of
        -- ways of entering insert mode
        EvKey (KChar 'a') [] -> NextState $ st {mode = InsertMode, cx = cx st + 1}
        EvKey (KChar 'i') [] -> NextState $ st {mode = InsertMode}
        EvKey (KChar 'o') [] -> do
          let curLines = lines st
              curY = cy st
              prevLines = take (curY + 1) curLines
              postLines = drop (curY + 1) curLines
              newLines = prevLines ++ [""] ++ postLines
          NextState $ st {mode = InsertMode, lines = newLines, cx = 0, cy = cy st + 1}
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
    InsertMode ->
      case e of
        EvKey KLeft [] -> NextState $ moveCursorLeft st
        EvKey KDown [] -> NextState $ moveCursorVert 1 st
        EvKey KUp [] -> NextState $ moveCursorVert (-1) st
        EvKey KRight [] -> NextState $ moveCursorRight st
        EvKey KBS [] | cx st == 0 && cy st > 0 -> do
          -- put cur line at end of prev line
          let curLines = lines st
              curY = cy st
              curLine = curLines !! curY
              prevLine = curLines !! (curY - 1)
              pre = take (curY - 1) curLines
              post = drop (curY + 1) curLines
              newLines = pre ++ [prevLine <> curLine] ++ post
          NextState $ st {lines = newLines, cx = length prevLine, cy = curY - 1}
        EvKey KBS [] | cx st > 0 -> do
          -- delete char from cur line
          let curLines = lines st
              curLine = curLines !! cy st
              x = cx st
              pre = take (x - 1) curLine
              post = drop x curLine
              newLine = pre ++ post
              newLines = modifyAt (cy st) (const newLine) curLines
          NextState $ st {lines = newLines, cx = x - 1}
        EvKey (KChar c) [] -> do
          -- insert char at cursor
          let curLines = lines st
              curLine = curLines !! cy st
              x = cx st
              pre = take x curLine
              post = drop x curLine
              newLine = pre ++ [c] ++ post
              newLines = modifyAt (cy st) (const newLine) curLines
          NextState $ st {lines = newLines, cx = x + 1}
        EvKey KEnter [] -> do
          -- insert newline (TODO: automatic indentation)
          let curLines = lines st
              curY = cy st
              curLine = curLines !! curY
              x = cx st
              pre = take x curLine
              post = drop x curLine
              prevLines = take curY curLines
              postLines = drop (curY + 1) curLines
              newLines = prevLines ++ [pre, post] ++ postLines
          NextState $ st {lines = newLines, cx = 0, cy = curY + 1}
        EvKey KEsc [] ->
          let
           in NextState $ ensureCXisInCurLineBounds $ st {mode = NormalMode}
        _ -> NextState st
    CommandMode s ->
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
moveCursorLeft st = ensureCXisInCurLineBounds $ st {cx = cx st - 1}

moveCursorRight :: State -> State
moveCursorRight st = ensureCXisInCurLineBounds $ st {cx = cx st + 1}

ensureCXisInCurLineBounds :: State -> State
ensureCXisInCurLineBounds st =
  let curLine = lines st !! cy st
   in st {cx = max 0 $ min (length curLine - 1) (cx st)}

moveCursorVert :: Int -> State -> State
moveCursorVert dy st =
  let curY = cy st
      newY = max 0 $ min (length (lines st) - 1) (curY + dy)
      newX =
        if newY == curY
          then cx st
          else
            let curLine = lines st !! newY
             in min (length curLine - 1) (cx st)
   in st {cx = newX, cy = newY}

modifyAt :: Int -> (a -> a) -> [a] -> [a]
modifyAt n f xs =
  let (before, after) = splitAt n xs
   in case after of
        [] -> xs
        (y : ys) -> before ++ f y : ys

draw :: Vty -> State -> IO ()
draw vty st = do
  (width, height) <- displayBounds (outputIface vty)
  let contentLines =
        lines st
          & take (height - 2)
          & map (string defAttr)
      statusLine = statusLineImage width height (length contentLines) st
      commandLine = commandLineImage width st
      img = [vertCat contentLines <-> statusLine <-> commandLine]
  vty.update $
    Picture
      { picCursor = Cursor (cx st) (cy st),
        picLayers = img,
        picBackground = ClearBackground
      }

statusLineImage :: Int -> Int -> Int -> State -> Image
statusLineImage width height nContentLines st =
  let (str, fg, bg) = case mode st of
        NormalMode -> ("NORMAL", black, green)
        InsertMode -> ("INSERT", black, blue)
        CommandMode _ -> ("COMMAND", black, magenta)
      padded = take width (str ++ repeat ' ')
      attr = (defAttr `withBackColor` bg) `withForeColor` fg
   in string attr padded
        & translateY (height - nContentLines - 2)

commandLineImage :: Int -> State -> Image
commandLineImage width st =
  let str = case mode st of
        CommandMode s -> ":" <> s
        _ -> ""
      padded = take width (str ++ repeat ' ')
   in string defAttr padded

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
