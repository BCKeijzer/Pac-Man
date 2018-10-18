module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.Color
import Drawing
import Control
import Step
import DTypes

main :: IO ()
main = play window background fps initialState render handleKeys update