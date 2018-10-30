module Main where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color
import Drawing
import Control
import Step
import DTypes

main :: IO ()
main = do highscores <- readFile "src/highscores.txt"
          playIO window background fps (initialState highscores) render handleKeys update