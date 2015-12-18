module Noise where

import System.Random
import Numeric.Noise 
import Numeric.Noise.Perlin

type NoiseGen = Int -> Int -> Int -> [Double]

data Universe = Universe Int Int [Double]

instance Show Universe where
    show (Universe _ cols cells)   = unlines $ toRows cols $ map toChar cells
        where toChar x | x > 0     = '*'
                       | otherwise = ' '

toRows :: Int -> [a] -> [[a]]
toRows _  [] = []
toRows cols xs =
  let s = splitAt cols xs
   in fst s : toRows cols (snd s)

createUniverse :: Int -> Int -> Int -> NoiseGen -> Universe
createUniverse seed rows cols noiseGen = Universe rows cols $ noiseGen seed rows cols

createPerlinData :: NoiseGen
createPerlinData seed rows cols = map (noiseValue (perlinGen (fst (random (mkStdGen seed))))) $ createGrid rows cols

createRidgedData :: NoiseGen
createRidgedData seed rows cols = map (noiseValue (ridgedGen (fst (random (mkStdGen seed))))) $ createGrid rows cols

createSphereData :: NoiseGen
createSphereData r rows cols = createSphere (fromIntegral r) $ createGrid rows cols

fuse :: Universe -> Universe -> Universe
fuse u (Universe _ _ cells) = addLayer u cells

addLayer :: Universe -> [Double] -> Universe
addLayer (Universe rows cols cells) ld = Universe rows cols $ zipWith (average) cells ld

average :: Double -> Double -> Double
average a b = (a + b) / 2

createGrid :: Int -> Int -> [Point]
createGrid rows cols = [ (x, y, 0) | y <- [0 .. fromIntegral (rows - 1)], x <- [0 .. fromIntegral (cols - 1)]]

createSphere :: Double -> [Point] -> [Double]
createSphere r grid = 
    let centerPoint = getCenterPoint grid
     in map (createSphere' r . distanceBetween centerPoint) grid

createSphere' :: Double -> Double -> Double
createSphere' r d = (r - d) / r

getCenterPoint :: [Point] -> Point
getCenterPoint ps = 
  let fstP = head ps
      lstP = last ps
   in halfWayPoint fstP lstP

halfWayPoint :: Point -> Point -> Point
halfWayPoint (x1, y1, z1) (x2, y2, z2) = ((x1 + x2) / 2, (y1 + y2) / 2, (z1 + z2) / 2)

distanceBetween :: Point -> Point -> Double
distanceBetween (x1, y1, z1) (x2, y2, z2) = sqrt ((x1 - x2)**2 + (y1 - y2)**2 + (z1 - z2)**2) 

octaves :: Int
octaves = 5

scale :: Double
scale = 0.05

persistance :: Double
persistance = 0.5

frequency :: Double
frequency = 1

lacunarity :: Double
lacunarity  = 2

perlinGen :: Int -> Perlin
perlinGen s = perlin s octaves scale persistance

ridgedGen :: Int -> Ridged
ridgedGen s = ridged s octaves scale frequency lacunarity

mapToString :: [Bool] -> String
mapToString = map mapToString'

mapToString' :: Bool -> Char
mapToString' True = '*'
mapToString' _    = '_'