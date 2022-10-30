{-# LANGUAGE OverloadedStrings #-}

module Pura.Parse
  ( Config (..),
    parse,
    load,
  )
where

import Control.Applicative
import qualified Data.ByteString as B
import qualified Data.Map as M
import Data.Yaml (FromJSON (..), ToJSON (..), object, (.:), (.:?), (.=))
import qualified Data.Yaml as Y

data Config = Config
  { packages :: Maybe [String],
    shellAliases :: Maybe (M.Map String String),
    shellHook :: Maybe String
  }
  deriving (Eq, Show)

instance FromJSON Config where
  parseJSON (Y.Object v) =
    Config
      <$> v .: "packages"
      <*> v .: "shellAliases"
      <*> v .: "shellHook"
  parseJSON _ = fail "Expected Object of Config value"

parse :: B.ByteString -> Maybe Config
parse = Y.decodeThrow

load :: FilePath -> IO (Maybe Config)
load p = do
  s <- B.readFile p
  return $ parse s
