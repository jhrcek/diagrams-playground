{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}
module Main where

import Diagrams.Backend.Rasterific.CmdLine (B, animMain)
import Diagrams.Prelude

main :: IO ()
main = animMain animation

animation :: Animation B V2 Double
animation =
     (rotateBy <$> ui <*> square 1 # lw veryThick)
     <> pure (square 2 # fc white)
