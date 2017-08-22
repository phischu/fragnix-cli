module Main where

import Init (fragnixInit)
import Build (fragnixBuild)

import Options.Applicative (
  ParserInfo, Parser, execParser,
  subparser, command, info,
  progDesc, header, metavar,
  many, argument, str)


data Command =
  Build [FilePath] |
  Init String

commandParserInfo :: ParserInfo Command
commandParserInfo =
  info commandParser (header "fragnix - fragment-based code distribution")

commandParser :: Parser Command
commandParser = subparser (mconcat [
  command "build" (info buildParser (progDesc "Build the given list of modules.")),
  command "init" (info initParser (progDesc "Download the given environment."))])

buildParser :: Parser Command
buildParser = Build <$> many (argument str (metavar "TARGET"))

initParser :: Parser Command
initParser = Init <$> argument str (metavar "ENVIRONMENT")

main :: IO ()
main = do
    command <- execParser commandParserInfo
    case command of
      Build modulePaths -> fragnixBuild modulePaths
      Init environmentName -> fragnixInit environmentName

