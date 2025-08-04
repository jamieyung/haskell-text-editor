{-# LANGUAGE OverloadedRecordDot #-}
module Render where

import Data.Function ((&))
import Graphics.Vty hiding (Mode)
import Prelude hiding (lines)
import State

draw :: Vty -> State -> IO ()
draw vty st = do
  (width, height) <- displayBounds (outputIface vty)
  let contentLines =
        st.lines
          & take (height - 2)
          & map (string defAttr)
      statusLine = statusLineImage width height (length contentLines) st
      commandLine = commandLineImage width st
      img = [vertCat contentLines <-> statusLine <-> commandLine]
  vty.update $
    Picture
      { picCursor = Cursor st.cx st.cy,
        picLayers = img,
        picBackground = ClearBackground
      }

statusLineImage :: Int -> Int -> Int -> State -> Image
statusLineImage width height nContentLines st =
  let (str, fg, bg) = case st.mode of
        NormalMode -> ("NORMAL", black, green)
        InsertMode -> ("INSERT", black, blue)
        CommandMode _ -> ("COMMAND", black, magenta)
      padded = take width (str ++ repeat ' ')
      attr = (defAttr `withBackColor` bg) `withForeColor` fg
   in string attr padded
        & translateY (height - nContentLines - 2)

commandLineImage :: Int -> State -> Image
commandLineImage width st =
  let str = case st.mode of
        CommandMode s -> ":" <> s
        _ -> ""
      padded = take width (str ++ repeat ' ')
   in string defAttr padded

