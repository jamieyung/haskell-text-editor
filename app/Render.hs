{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NamedFieldPuns #-}

module Render where

import Data.Function ((&))
import Graphics.Vty hiding (Mode)
import State
import Prelude hiding (lines)

draw :: Vty -> State -> IO ()
draw vty st = do
  (width, height) <- displayBounds (outputIface vty)
  let contentLines =
        st
        -- TODO: can we avoid the need for this transformation
          & \_ -> map toLine (above st) <> [toLine $ cur st] <> map toLine (below st)
          & take (height - 2)
          & map (string defAttr)
      statusLine = statusLineImage width height (length contentLines) st
      commandLine = commandLineImage width st
      img = [vertCat contentLines <-> statusLine <-> commandLine]
  vty.update $
    Picture
      { picCursor = uncurry Cursor (cursorPos st),
        picLayers = img,
        picBackground = ClearBackground
      }
  where
    toLine Line {before, after} = before <> after

    cursorPos :: State -> (Int, Int)
    cursorPos State {above, cur = Line {before}} = (length before, length above)

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
