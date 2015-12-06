module Helper (bezierPath) where

import System.Random
import Graphics.Rasterific

instance (Random x, Random y) => Random (x, y) where
  randomR ((x1, y1), (x2, y2)) gen1 =
    let (x, gen2) = randomR (x1, x2) gen1
        (y, gen3) = randomR (y1, y2) gen2
    in ((x, y), gen3)

pointX :: Point -> Float
pointX (V2 x _) = x

pointY :: Point -> Float
pointY (V2 _ y) = y

toPoint :: (Float, Float) -> Point
toPoint (x, y) = V2 x y

bezierPath :: RandomGen g => g -> Float -> Point -> Point -> [Bezier]
bezierPath gen deviation begin end =
    let segments = fst $ randomR (10, 25) gen :: Int
     in pointsToBeziers gen deviation $ nudgeStops gen deviation $ generateStops segments begin end

generateStops :: Int -> Point -> Point -> [Point]
generateStops segments begin end = zipWith V2 (setOf pointX begin end segments) (setOf pointY begin end segments) ++ [end]

nudgeStops :: RandomGen g => g -> Float -> [Point] -> [Point]
nudgeStops gen deviation stops = head stops : zipWith (+) (map toPoint $ randomRs ((-deviation, -deviation), (deviation, deviation)) gen) ((tail . init) stops) ++ [last stops]

pointTripletToBezier ::  [Point] -> Bezier
pointTripletToBezier [s, c, p] = Bezier s c p
pointTripletToBezier _         = pointTripletToBezier $ replicate 3 (V2 0 0)

pointsToBeziers :: RandomGen g => g -> Float -> [Point] -> [Bezier]
pointsToBeziers gen deviation (x:y:xs) =
    let triplet = nudgeStops gen deviation $ generateStops 2 x y
     in pointTripletToBezier triplet : pointsToBeziers gen deviation (y:xs)
pointsToBeziers _ _ _ = []

setOf :: (Point -> Float) -> Point -> Point -> Int -> [Float]
setOf coordinatePicker begin end segments = 
    let begin' = coordinatePicker begin
        end'   = coordinatePicker end
        step   = (end' - begin') / fromIntegral segments
     in take segments $ iterate (+step) begin'