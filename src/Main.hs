module Main where

import           Diagrams.Backend.SVG
import           Diagrams.Prelude
import           Net2Diagram
import           ProofNets

main :: IO ()
main = writeGraph testPN2

main2 :: PN i o -> IO ()
main2 = writeGraph

main3 :: Diagram SVG -> IO ()
main3 = writeDiagram
