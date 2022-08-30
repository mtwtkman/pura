module Main where

import Pura.Build
import Pura.Parse
import System.FilePath (joinPath, (<.>))
import Options.Applicative

type TemplateRoot = String

write :: FilePath -> Config -> IO ()
write p c = writeFile p (buildFromConfig c)

data PuraCommand = TemplateSpecified
                    { getTemplateName :: String
                    , getTemplateRoot :: String
                    }
                 deriving (Show)

buildTemplatePath :: TemplateRoot -> PuraCommand -> FilePath
buildTemplatePath t (TemplateSpecified n _) = joinPath [t,n]

defaultTemplateRoot :: FilePath
defaultTemplateRoot = "~/.config/pura"

puraCommand :: Parser PuraCommand
puraCommand = TemplateSpecified
  <$> argument str (metavar "TEMPLATE_NAME")
  <*> option str ( long "template-root"
             <> short 'r'
             <> value defaultTemplateRoot
             <> metavar "TEMPLATE_ROOT_DIR_NAME"
             )

parsePuraCommand :: IO PuraCommand
parsePuraCommand = execParser opts
  where
    opts = info (puraCommand <**> helper)
      ( fullDesc
        <> progDesc "shell.nix generator"
        <> header "Pura - helps you generating shell.nix with template system" )

main :: IO ()
main = do
  parsed <- parsePuraCommand
  let templateRoot = getTemplateRoot parsed
      templatePath = buildTemplatePath templateRoot parsed
      templatePathCandidates = map (templatePath <.>) ["yaml", "yml"]
  content <- attemptLoad templatePathCandidates
  print content
  where
    attemptLoad (x:xs) = do
      content <- load x
      case content of
        Just _ -> return content
        Nothing -> attemptLoad xs
