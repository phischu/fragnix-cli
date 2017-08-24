module Main where

import Build (fragnixBuild)
import Init (fragnixInit)
import Fetch (fragnixFetch)

import Fragnix.Slice (
  SliceID)

import Options.Applicative (
  ParserInfo, Parser, execParser,
  subparser, command, info,
  progDesc, header, metavar,
  many, argument, str, auto)


data Command =
  Build [FilePath] |
  Init String |
  Fetch SliceID

commandParserInfo :: ParserInfo Command
commandParserInfo =
  info commandParser (header "fragnix - fragment-based code distribution")

commandParser :: Parser Command
commandParser = subparser (mconcat [
  command "build" (info buildParser (progDesc "Build the given list of modules.")),
  command "init" (info initParser (progDesc "Download the given environment.")),
  command "fetch" (info fetchParser (progDesc "Fetch the slice with the given sliceID."))])

buildParser :: Parser Command
buildParser = Build <$> many (argument str (metavar "TARGET"))

initParser :: Parser Command
initParser = Init <$> argument str (metavar "ENVIRONMENT")

fetchParser :: Parser Command
fetchParser = Fetch <$> argument auto (metavar "SLICEID")

main :: IO ()
main = do
    command <- execParser commandParserInfo
    case command of
      Build modulePaths -> fragnixBuild modulePaths
      Init environmentName -> fragnixInit environmentName
      Fetch sliceID -> fragnixFetch sliceID

