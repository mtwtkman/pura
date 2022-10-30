module Pura.Build
  ( buildNixShell,
    Packages (..),
    Aliases (..),
    Alias (..),
    ShellHook,
    defaultPackages,
    defaultAliases,
    defaultShellHook,
    indent,
    concatLine,
    buildFromConfig,
  )
where

import Control.Applicative
import Data.List (intercalate)
import qualified Data.Map as M
import Data.Maybe
import Data.Semigroup ((<>))
import Pura.Parse

indent :: Int -> String -> String
indent i = (++) (intercalate "" $ replicate i " ")

concatLine :: [String] -> String
concatLine = intercalate "\n"

newtype Packages = Packages
  { getPackages :: [String]
  }

defaultPackages :: Packages
defaultPackages = Packages []

data Alias = Alias
  { getAliasName :: String,
    getAliasValue :: String
  }

instance Show Alias where
  show a = "alias " ++ getAliasName a ++ "=\"" ++ getAliasValue a ++ "\""

newtype Aliases = Aliases
  { getAliases :: [Alias]
  }

aliasesFromMap :: M.Map String String -> Aliases
aliasesFromMap m = Aliases (M.foldlWithKey (\acc k v -> acc ++ [Alias k v]) [] m)

defaultAliases :: Aliases
defaultAliases = Aliases []

type ShellHook = String

defaultShellHook :: ShellHook
defaultShellHook = ""

buildNixShell :: Packages -> Aliases -> ShellHook -> String
buildNixShell p a h =
  unlines
    [ "with import <nixpkgs> {};",
      "mkShell {",
      indent 2 "packages = [",
      concatLine (map (indent 4) $ getPackages p),
      indent 2 "];",
      indent 2 "shellHook = ''",
      concatLine (map (indent 4 . show) $ getAliases a),
      concatLine (map (indent 4) $ lines h),
      indent 2 "'';",
      "}"
    ]

buildFromConfig :: Config -> String
buildFromConfig c =
  let p = maybe defaultPackages Packages (packages c)
      a = maybe defaultAliases aliasesFromMap (shellAliases c)
      h = fromMaybe defaultShellHook (shellHook c)
   in buildNixShell p a h
