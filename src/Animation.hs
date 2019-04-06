module Main where

import Diagrams.Backend.Rasterific (GifDelay)
import Diagrams.Backend.Rasterific.CmdLine (B, uniformGifMain)
import Diagrams.Prelude

main :: IO ()
main =
    uniformGifMain (2 :: GifDelay) $ rotatingSquares <$> [1..90]
  where
    rotatingSquares :: Double -> Diagram B
    rotatingSquares degrees = mconcat
      [ square 1         # rot 8
      , square s         # rot 4
      , square (s*s)     # rot 2
      , square (s*s*s)   # rot 1
      , square (s*s*s*s) # fc white
      ] # lw thick
        where
          rot :: Double -> Diagram B -> Diagram B
          rot x = rotate (x * degrees @@ deg)

          s :: Double
          s = sqrt 2
