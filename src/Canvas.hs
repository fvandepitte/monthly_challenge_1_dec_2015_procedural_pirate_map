module Canvas(draw) where

import Helper

import System.Random
import Codec.Picture( PixelRGBA8( .. ))
import Graphics.Rasterific
import Graphics.Rasterific.Texture

import Constants as C

intPairToPoit :: Int -> Int -> Point
intPairToPoit x y = V2 (fromIntegral x) (fromIntegral y)

draw :: RandomGen g => g -> Drawing PixelRGBA8 ()
draw gen =  do
    let d = fromIntegral C.canvasBorder * 2 / 3 ::Float
        bezierPath' = bezierPath gen d
        p = concatMap (uncurry bezierPath') [ (intPairToPoit C.canvasBorder C.canvasBorder, intPairToPoit C.canvasBorder (C.height - C.canvasBorder)), (intPairToPoit C.canvasBorder (C.height - C.canvasBorder), intPairToPoit (C.width - C.canvasBorder) (C.height - C.canvasBorder)), (intPairToPoit (C.width - C.canvasBorder) (C.height - C.canvasBorder), intPairToPoit (C.width - C.canvasBorder) C.canvasBorder), (intPairToPoit (C.width - C.canvasBorder) C.canvasBorder, intPairToPoit C.canvasBorder C.canvasBorder) ]
    
    withTexture (uniformTexture C.black)    $ stroke 5 JoinRound (CapRound, CapRound) p
    withTexture (uniformTexture C.seaColor) $ fill p
    withTexture (uniformTexture C.mapColor) $ stroke 5 JoinRound (CapRound, CapRound) p