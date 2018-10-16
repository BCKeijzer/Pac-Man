module Main where

import Graphics.Gloss
import Drawing
import Control
import Step
import DTypes

main :: IO ()
main = play window background fps initialState render handleKeys update
