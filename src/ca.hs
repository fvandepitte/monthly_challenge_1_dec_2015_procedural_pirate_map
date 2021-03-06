module CA where

import System.Random
import Data.List
import Control.Parallel.Strategies (rseq, parMap)



type Coord = (Int, Int)
type Cell  = (Coord, Bool)
type Rule  = (Cell -> [Cell] -> Cell)
data Universe = Universe Int Int [Cell]

getCells :: Universe -> [Cell]
getCells (Universe _ _ cells) = cells

instance Show Universe where
    show (Universe _ cols cells) = unlines $ toRows cols $ parMap rseq toChar cells
        where toRows _    []     = []
              toRows cls cells'  = take cls cells' : (toRows cls $ drop cls cells')
              toChar (_ , True)  = '*'
              toChar (_ , False) = ' '

nextGeneration :: Rule -> Universe -> Universe
nextGeneration rule u = nextGeneration'' u $ parMap rseq (nextGeneration' rule) $ parMap rseq (\c -> (c, getNeighbours c u))  $ getCells u

nextGeneration' :: Rule -> (Cell, [Cell]) -> Cell
nextGeneration' rule (c, cells) = rule c cells

nextGeneration'' :: Universe -> [Cell] -> Universe
nextGeneration'' (Universe rows cols _) cells = Universe rows cols cells

getNeighbours :: Cell -> Universe -> [Cell]
getNeighbours ((x, y), _) u = [ getCell (x+x', y+y') u | x' <- [-1 .. 1], y' <- [-1 .. 1], x' /= 0 || y' /=0 ]

getCell :: Coord -> Universe -> Cell
getCell (x, y) (Universe _ cols cells) | x < 0 || y < 0                = ((-1,-1), False)
                                       | (x + y * cols) - 1 > length cells = ((-1,-1), False)
                                       | otherwise                     = cells !! (x + y * cols)

toCell :: Coord -> Bool -> Cell
toCell c s = (c, s)

createUniverse :: RandomGen g => g -> Int -> Int -> Universe
createUniverse gen rows columns = 
    let coords = [ (x, y) | x <- [0 .. columns], y <- [0 .. rows] ]
     in Universe rows columns $ zipWith (toCell) coords (randoms gen)
