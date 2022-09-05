module Main where

import Control.Monad
import Data.List
import Options.Applicative
import Pura.Build
import Pura.Parse
import System.Directory
import System.FilePath (joinPath, (<.>))
import System.Posix.User

type TemplateRoot = String

type TemplateName = String

write :: FilePath -> Config -> IO ()
write p c = writeFile p (buildFromConfig c)

data PuraCommand = TemplateSpecified
  { getTemplateRoot :: TemplateRoot,
    getTemplateName :: TemplateName
  }
  deriving (Show)

buildTemplatePath :: PuraCommand -> FilePath
buildTemplatePath (TemplateSpecified r n) = joinPath [r, n]

defaultTemplateRoot :: FilePath
defaultTemplateRoot = "~/.config/pura"

puraCommand :: Parser PuraCommand
puraCommand =
  TemplateSpecified
    <$> option
      str
      ( long "template-root"
          <> short 'r'
          <> value defaultTemplateRoot
          <> metavar "TEMPLATE_ROOT_DIR_NAME"
      )
    <*> argument str (metavar "TEMPLATE_NAME")

parsePuraCommand :: IO PuraCommand
parsePuraCommand = execParser opts
  where
    opts =
      info
        (puraCommand <**> helper)
        ( fullDesc
            <> progDesc "shell.nix generator"
            <> header "Pura - helps you generating shell.nix with template system"
        )

matchedTemplateFile :: [FilePath] -> IO [FilePath]
matchedTemplateFile xs = do
  paths <- mapM expandHomeDir xs
  filterM doesPathExist paths

outputName :: String
outputName = "shell.nix"

startsWithTilde :: String -> Bool
startsWithTilde s = head s == '~'

expandHomeDir :: FilePath -> IO FilePath
expandHomeDir ('~' : xs) = do
  x <- getHomeDirectory
  return $ x ++ xs
expandHomeDir s = return s

main :: IO ()
main = do
  parsed <- parsePuraCommand
  let templatePath = buildTemplatePath parsed
      templatePathCandidates = map (templatePath <.>) ["yaml", "yml"]
  matchedFiles <- matchedTemplateFile templatePathCandidates
  case matchedFiles of
    [] -> fail "Template file not found"
    [x] -> do
      content <- load x
      case content of
        Just def -> write outputName def
        Nothing -> fail "Cannot parse"
    xs -> fail $ multipleTemplateNameErroMsg xs
  where
    multipleTemplateNameErroMsg xs = "Found same named template files. (" ++ intercalate "," xs ++ ")"
