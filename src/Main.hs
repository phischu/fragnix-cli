module Main where

import Build (fragnixBuild)
import Init (fragnixInit)
import Fetch (fragnixFetch)
import Publish (fragnixPublish)

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
  Fetch SliceID |
  Publish String String

commandParserInfo :: ParserInfo Command
commandParserInfo =
  info commandParser (header "fragnix - fragment-based code distribution")

commandParser :: Parser Command
commandParser = subparser (mconcat [
  command "build" (info buildParser (
    progDesc "Build the given list of modules.")),
  command "init" (info initParser (
    progDesc "Download the given environment.")),
  command "fetch" (info fetchParser (
    progDesc "Fetch the slice with the given sliceID.")),
  command "publish" (info publishParser (
    progDesc "Publish the slice referred to the given module name and name."))])

buildParser :: Parser Command
buildParser = Build <$> many (argument str (metavar "TARGET"))

initParser :: Parser Command
initParser = Init <$> argument str (metavar "ENVIRONMENT")

fetchParser :: Parser Command
fetchParser = Fetch <$> argument auto (metavar "SLICEID")

publishParser :: Parser Command
publishParser = Publish <$> argument str (metavar "MODULENAME") <*> argument str (metavar "NAME")

main :: IO ()
main = do
    command <- execParser commandParserInfo
    case command of
      Build modulePaths -> fragnixBuild modulePaths
      Init environmentName -> fragnixInit environmentName
      Fetch sliceID -> fragnixFetch sliceID
      Publish moduleName name -> fragnixPublish moduleName name

