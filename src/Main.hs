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
main = mainWith $ pad 1.1 dlps

dlps :: Diagram B
dlps =
    (dots <> grid) # rotate (45 @@ deg)
  where
    line = unitSquare # replicate 5 # hcat
    grid = line # replicate 5 # vcat # centerXY
    dot = circle 0.07 # fc white
    dots = position [(p2 (x,y), dot) | x <- [0..5], y <- [0..5]] # centerXY

circlesWithNumbers :: Int -> Diagram B
circlesWithNumbers count = pad 1.1 $ vsep 0.2 $ fmap (\x -> text (show x) <> unitCircle) [1..count]

twoCirclesConnectedByArrow :: Diagram B
twoCirclesConnectedByArrow = (circledText n1 ||| strutX 2 ||| circledText n2)
    # connectOutside n1 n2
    # lw thick
  where
    n1 = 1 :: Int
    n2 = 2 :: Int

    circledText :: Int -> Diagram B
    circledText n = (text (show n) <> circle 0.6) # named n

completeGraphK6 :: Diagram B
completeGraphK6 = connectAllVertices $ hsep 0.5
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
