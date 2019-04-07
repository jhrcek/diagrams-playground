module Main where

import DiagonalLatticePaths
import Diagrams.Backend.Rasterific (GifDelay)
import Diagrams.Backend.Rasterific.CmdLine (B, uniformGifMain)
import Diagrams.Prelude

main :: IO ()
main = do
    putStrLn "Generating diagram"
    uniformGifMain 20 latticePaths

latticePaths :: [Diagram B]
latticePaths = zipWith drawWithinGrid [1..] (allDLPs 3)

drawAsLine :: DLP -> Diagram B
drawAsLine dlp =  dlp
    # toOffsets
    # fromOffsets
    # lineColor red
    # lineWidth veryThick
    # alignBR

drawWithinGrid :: Int -> DLP -> Diagram B
drawWithinGrid index dlp = mconcat
    [ text (show index) # fc black # scale 0.5 # translate (r2 (1.6,-1.6))
    , (drawAsLine dlp `atop` rotatedGrid) # rotate (-45 @@ deg) # centerXY
    , square 4.5 # fc white
    ]
  where
    rotatedGrid = grid
        # lineColor black
        # lineWidth thin
        # alignBR

    grid = hcat $ replicate (getN dlp) line
    line = vcat $ replicate (getN dlp) unitSquare
    getN (DLP steps) = length steps `div` 2
