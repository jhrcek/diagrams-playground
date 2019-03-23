module DiagonalLatticePaths
    ( allDLPs
    , drawAsLine
    , drawWithinGrid
    , DLP
    , Step(..)
    ) where

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import           Diagrams.TwoD
import           Math.Combinat.Numbers.Sequences (binomial)

data Step = Down | Up deriving (Eq, Show, Ord, Enum, Bounded)

-- Diagonal Lattice Path
data DLP = DLP [Step] deriving Show

least :: Int -> DLP
least n
    | n < 0 = error $ "minDLP negative argument" <> show n
    | otherwise = DLP $ replicate n Down ++ replicate n Up

allDLPs :: Int -> [DLP]
allDLPs n = take dlpCount . iterate next $ least n
  where
    dlpCount = fromIntegral $ binomial (2*n) n

next :: DLP -> DLP
next (DLP dlp) = DLP . fst $ foldr f ([], False) dlp
  where
    f Down ((Up:xs),False) = (Up:Down:reverse xs, True)
    f s (xs, b)            = (s:xs,b)

toOffsets :: DLP -> [V2 Double]
toOffsets (DLP steps) = toOffset <$> steps

toOffset :: Step -> V2 Double
toOffset Down = V2 1 (-1)
toOffset Up   = V2 1 1

drawAsLine :: DLP -> Diagram B
drawAsLine dlp = (toOffsets dlp)
    # fromOffsets
    # lineWidth thick
    # lineColor red

drawWithinGrid :: DLP -> Diagram B
drawWithinGrid dlp = drawAsLine dlp `atop` rotatedGrid
  where
    rotatedGrid = grid # rotateBy (1/8)
                       # scale (sqrt 2)
                       # translateX 1
                       # lineColor gray
    grid = hcat $ replicate (getN dlp) line
    line = vcat $ replicate (getN dlp) (square 1)
    getN (DLP steps) = length steps `div` 2
