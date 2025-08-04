module Main where

import Data.Function ((&))
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
  case mode st of
    NormalMode ->
      case e of
        EvKey (KChar 'i') [] -> eventLoop vty $ st {mode = InsertMode}
        EvKey (KChar 'h') [] -> eventLoop vty $ st {cx = max 0 (cx st - 1)}
        EvKey (KChar 'j') [] -> eventLoop vty $ moveCursorVert 1 st
        EvKey (KChar 'k') [] -> eventLoop vty $ moveCursorVert (-1) st
        EvKey (KChar 'l') [] ->
          let curLine = lines st !! cy st
           in eventLoop vty $ st {cx = min (length curLine - 1) (cx st + 1)}
        EvKey KEsc [] -> shutdown vty
        _ -> eventLoop vty st
    InsertMode ->
      case e of
        EvKey KBS [] | cx st == 0 && cy st > 0 -> do
          -- put cur line at end of prev line
          let curLines = lines st
              curY = cy st
              curLine = curLines !! curY
              prevLine = curLines !! (curY - 1)
              pre = take (curY - 1) curLines
              post = drop (curY + 1) curLines
              newLines = pre ++ [prevLine <> curLine] ++ post
          eventLoop vty $ st {lines = newLines, cx = length prevLine, cy = curY - 1}
        EvKey KBS [] | cx st > 0 -> do
          -- delete char from cur line
          let curLines = lines st
              curLine = curLines !! cy st
              x = cx st
              pre = take (x - 1) curLine
              post = drop x curLine
              newLine = pre ++ post
              newLines = modifyAt (cy st) (const newLine) curLines
          eventLoop vty $ st {lines = newLines, cx = x - 1}
        EvKey (KChar c) [] -> do
          -- insert char at cursor
          let curLines = lines st
              curLine = curLines !! cy st
              x = cx st
              pre = take x curLine
              post = drop x curLine
              newLine = pre ++ [c] ++ post
              newLines = modifyAt (cy st) (const newLine) curLines
          eventLoop vty $ st {lines = newLines, cx = x + 1}
        -- EvKey KEnter [] -> eventLoop vty (State $ s ++ ['\n'])
        EvKey KEsc [] -> eventLoop vty $ st {mode = NormalMode}
        _ -> eventLoop vty st

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
  let content =
        lines st
          & take (height - 1)
          & map (string defAttr)
          & vertCat
      statusLine = statusLineImage width height st
      img = [content <-> statusLine]
  update vty $
    Picture
      { picCursor = Cursor (cx st) (cy st),
        picLayers = img,
        picBackground = ClearBackground
      }

statusLineImage :: Int -> Int -> State -> Image
statusLineImage width height st =
  let (str, fg, bg) = case mode st of
        NormalMode -> ("NORMAL", black, green)
        InsertMode -> ("INSERT", black, blue)
      padded = take width (str ++ repeat ' ')
      attr = (defAttr `withBackColor` bg) `withForeColor` fg
   in string attr padded
        & translateY (height - 3)

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

data Mode = NormalMode | InsertMode
