{-# LANGUAGE NegativeLiterals #-}
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
toOffset Down = unitX
toOffset Up   = unitY

drawAsLine :: DLP -> Diagram B
drawAsLine dlp =  dlp
    # toOffsets
    # fromOffsets
    # lineColor red
    # alignBR


drawWithinGrid :: DLP -> Diagram B
drawWithinGrid dlp = (drawAsLine dlp `atop` rotatedGrid) # rotate (-45 @@ deg)
  where
    rotatedGrid = grid
        # lineColor gray
        # alignBR

    grid = hcat $ replicate (getN dlp) line
    line = vcat $ replicate (getN dlp) unitSquare
    getN (DLP steps) = length steps `div` 2
