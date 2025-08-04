{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Graphics.Vty hiding (Mode)
import Graphics.Vty.Platform.Unix (mkVty)
import Prelude hiding (lines)
import Render
import State
import Update

main :: IO ()
main = do
  vty <- mkVty defaultConfig
  eventLoop vty initialState

eventLoop :: Vty -> State -> IO ()
eventLoop vty st = do
  draw vty st
  e <- nextEvent vty
  case Update.update e st of
    Quit -> shutdown vty
    NextState st' -> eventLoop vty st'
