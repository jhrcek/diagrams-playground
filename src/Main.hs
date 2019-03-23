{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import           Diagrams.Backend.SVG.CmdLine
import           Diagrams.Prelude
import DiagonalLatticePaths -- (allDLPs, drawAsLine)

main :: IO ()
main = mainWith ex4

ex1 :: Diagram B
ex1 = circle 1

ex2 :: Diagram B
ex2 = vcat (replicate 3 circles)
  where
    circles = hcat (map circle [1..6])

ex3 :: Diagram B
ex3 = ell ||| vrule 2 ||| ell
 where
   ell = circle 1 # scaleX 0.5 # rotateBy (1/6)

ex4 :: Diagram B
ex4 = vsep 1 $ drawWithinGrid <$> allDLPs 3
