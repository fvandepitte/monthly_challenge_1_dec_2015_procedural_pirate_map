module Main where

import Constants as C
import Canvas

import System.Random
import Codec.Picture( writePng )
import Graphics.Rasterific

main :: IO ()
main = do
  gen <- getStdGen
  let img = renderDrawing C.width C.height C.white $ Canvas.draw gen
         

  writePng "yourimage.png" img
