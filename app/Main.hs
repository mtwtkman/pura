module Main where

import Pura.Build
import Pura.Parse

main :: IO ()
main = do
  content <- load "./example/hs.yaml"
  case content of
    Just config -> putStr $ buildFromConfig config
    Nothing -> print "cannot load"

