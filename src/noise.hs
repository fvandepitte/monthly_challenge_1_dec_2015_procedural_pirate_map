module Noise where

import System.Random
import Data.List (last)
import Numeric.Noise 
import Numeric.Noise.Perlin


data Universe = Universe Int Int [Bool]

instance Show Universe where
    show (Universe _ cols cells) = unlines $ toRows cols $ map toChar cells
        where toRows _    []     = []
              toRows cls cells'  = take cls cells' : (toRows cls $ drop cls cells')
              toChar True  = '*'
              toChar False = ' '

createUniverse :: RandomGen g => g -> Int -> Int -> Double -> Universe
createUniverse gen rows cols r =
    let grid   = [ (x, y, 0) | x <- [0 .. fromIntegral cols], y <- [0 .. fromIntegral rows]]
        sphere = createSphere rows cols r grid
     in Universe rows cols $ map (>0) $ zipWith (+) sphere $ zipWith (*) sphere $ map (noiseValue (perlinGen (fst (random gen)))) $ grid

createSphere :: Int -> Int -> Double -> [Point] -> [Double]
createSphere rows cols r grid = 
    let centerPoint = getCenterPoint grid
     in map (createSphere' r . distanceBetween centerPoint) grid

createSphere' :: Double -> Double -> Double
createSphere' r d | d > r     = 0
                  | otherwise = r - d

getCenterPoint :: [Point] -> Point
getCenterPoint ps = 
  let fstP = head ps
      lstP = last ps
   in halfWayPoint fstP lstP

halfWayPoint :: Point -> Point -> Point
halfWayPoint (x1, y1, z1) (x2, y2, z2) = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2)

distanceBetween :: Point -> Point -> Double
distanceBetween (x1, y1, z1) (x2, y2, z2) = sqrt ((x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2) 

octaves :: Int
octaves = 5

scale :: Double
scale = 0.05

persistance :: Double
persistance = 0.5

perlinGen :: Int -> Perlin
perlinGen s = perlin s octaves scale persistance

mapToString :: [Bool] -> String
mapToString = map mapToString'

mapToString' :: Bool -> Char
mapToString' True = '*'
mapToString' _    = '_'