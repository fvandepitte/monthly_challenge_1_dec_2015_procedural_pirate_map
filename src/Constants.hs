module Constants where

import Codec.Picture( PixelRGBA8( .. ))

height :: Int
height = 360

width :: Int
width = 640

canvasBorder :: Int
canvasBorder = 15

white :: PixelRGBA8
white = PixelRGBA8 255 255 255 255

black :: PixelRGBA8
black = PixelRGBA8 0 0 0 255

mapColor :: PixelRGBA8
mapColor = PixelRGBA8 181 133 92 178

seaColor :: PixelRGBA8
seaColor = PixelRGBA8 103 163 214 255