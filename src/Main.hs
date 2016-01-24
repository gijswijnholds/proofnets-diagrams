module Main where

import           Net2Diagram
import           ProofNets

main :: IO ()
main = writeGraph testPN2

main2 :: PN i o -> IO ()
main2 = writeGraph
