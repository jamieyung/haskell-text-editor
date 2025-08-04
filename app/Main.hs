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
        EvKey (KChar 'j') [] -> eventLoop vty $ st {cy = min (length (lines st) - 1) (cy st + 1)}
        EvKey (KChar 'k') [] -> eventLoop vty $ st {cy = max 0 (cy st - 1)}
        EvKey (KChar 'l') [] -> do
          let curLine = lines st !! cy st
          eventLoop vty $ st {cx = min (length curLine - 1) (cx st + 1)}
        EvKey KEsc [] -> shutdown vty
        _ -> eventLoop vty st
    InsertMode ->
      case e of
        EvKey (KChar _) [] -> eventLoop vty st
        -- EvKey KEnter [] -> eventLoop vty (State $ s ++ ['\n'])
        EvKey KEsc [] -> eventLoop vty $ st {mode = NormalMode}
        _ -> eventLoop vty st

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
