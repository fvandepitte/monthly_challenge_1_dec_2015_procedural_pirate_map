module CA where

import System.Random
import Control.Monad
import Data.Maybe
import Data.List

type Coord = (Int, Int)
type Cell  = (Coord, Bool)
type Rule  = (Cell -> [Cell] -> Cell)
data Universe = Universe {
    rows  :: Int
  , cols  :: Int
  , cells :: [Cell]
}

instance Show Universe where
    show (Universe _ cols cells) = unlines $ toRows cols $ map toChar cells
        where toRows _    []     = []
              toRows cols cells  = take cols cells : (toRows cols $ drop cols cells)
              toChar (_ , True)  = '*'
              toChar (_ , False) = ' '

nextGeneration :: Rule -> Universe -> Universe
nextGeneration rule (Universe _ _ cells) = undefined

getCell :: Coord -> Universe -> Cell
getCell (x, y) u =  getCell' $ find (\((x', y'), _) -> x == x' && y == y') $ cells u
    where getCell' (Just c)  = c
          getCell' Nothing =  ((-1,-1), False)

toCell :: Coord -> Bool -> Cell
toCell c s = (c, s)

createUniverse :: RandomGen g => g -> Int -> Int -> Universe
createUniverse gen columns rows = 
    let coords = [ (x, y) | x <- [0 .. columns], y <- [0 .. rows] ]
     in Universe rows columns $ zipWith (toCell) coords (randoms gen)
