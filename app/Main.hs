module Main where

import Pura.Build
import Pura.Parse

write :: FilePath -> Config -> IO ()
write p c = writeFile p (buildFromConfig c)

main :: IO ()
main = do
  content <- load "./example/hs.yaml"
  case content of
    Just config -> write "./example/hoge.shell.nix" config
    Nothing -> print "cannot load"

