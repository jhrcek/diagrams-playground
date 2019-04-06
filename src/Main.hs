{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import Control.Monad (guard)
import DiagonalLatticePaths
import Diagrams.Backend.SVG.CmdLine
import Diagrams.Prelude

main :: IO ()
main = mainWith ex8

ex4 :: Diagram B
ex4 = vsep 1 $ drawWithinGrid <$> allDLPs 3

ex0 :: Diagram B
ex0 = cubicSpline True $ map p2
  [(98,100),(57,57),(59,45),(43,55),(79,64),(37,3),(5,13),(88,4)
  ,(2,51),(69,9),(3,68),(46,19),(82,72),(77,56),(46,78),(94,27),(31,26)]

ex1 :: Diagram B
ex1 =
    (mconcat $ map (cubicSpline True) vs) <> ((concat vs) `atPoints` (repeat $ circle 0.2 # fc blue))
  where
    vs = pathVertices . star (StarSkip 21)  $ regPoly 47 5

ex2 :: Diagram B
ex2 = pad 1.1 $ vsep 0.2 $ fmap (\x -> showOrigin $ text (show x) <> unitCircle) [1..100]

circledText :: Int -> Diagram B
circledText n = (text (show n) <> circle 0.6) # named n

ex3 :: Diagram B
ex3 = circledText n1 ||| strutX 2 ||| circledText n2
    & connectOutside n1 n2
    & lw veryThick
  where
    n1 = 1 :: Int
    n2 = 2 :: Int

ex5 :: Diagram B
ex5 = unitSquare # showEnvelope

ex6 :: Diagram B
ex6 = (arc (direction unitX) a
  ||| arc (direction unit_X) a
  ||| arc (direction unitX) a) # centerX
  <> arc' 3 (direction unit_X) a
  where a = 180 @@ deg

ex7 :: Diagram B
ex7 = [alignL, alignR, alignT, alignB, alignTL, alignTR, alignBL, alignBR, centerX, centerY, centerXY ]
  # fmap (\a -> unitSquare # a # showOrigin # showEnvelope)
  # vcat

ex8 :: Diagram B
ex8 = connectAllVertices $ hsep 0.5
    [ "left"  .>> (visPoints (trailVertices $ poly 5) <> c # named (6::Int))
    , "right" .>> (visPoints (trailVertices $ poly 6))
    ]
  where
    visPoints :: [P2 Double] -> Diagram B
    visPoints pts = atPoints pts (zipWith named vertexNames (repeat c))

    connectAllVertices :: Diagram B -> Diagram B
    connectAllVertices = applyAll $ do {- list monad -}
        subName <- ["left", "right"]
        a <- vertexNames
        b <- vertexNames
        guard (a < b)
        return $ connectOutside' (with & arrowHead .~ noHead) (subName .> a) (subName .> b)

    poly :: Int -> Located (Trail V2 Double)
    poly vertexCount = polygon (with & polyType .~ PolyRegular vertexCount 1)

    vertexNames :: [Int]
    vertexNames = [1..6]

    c :: Diagram B
    c = circle 0.05
