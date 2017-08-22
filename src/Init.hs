{-# LANGUAGE OverloadedStrings #-}
module Init where

import Network.Wreq (
  getWith, defaults, header,
  asJSON, responseBody)

import Paths_fragnix_cli (
  getDataDir)
import Fragnix.Environment (
  persistEnvironment, environmentPath, builtinEnvironmentPath)
import Language.Haskell.Names (
  Environment, Symbol)
import Language.Haskell.Exts (
  ModuleName(ModuleName))

import qualified Data.Map as Map (
  fromList)
import Control.Lens (
  (&), view, (.~))
import System.Directory (
  listDirectory, createDirectoryIfMissing, copyFile)
import Data.Traversable (
  for)


fragnixInit :: String -> IO ()
fragnixInit environmentName = do

  putStrLn ("Initializing environment " ++ environmentName)

  builtinEnvironment <- getEnvironment "builtin_environment"
  environment <- getEnvironment environmentName

  persistEnvironment builtinEnvironmentPath builtinEnvironment
  persistEnvironment environmentPath environment

  dataDir <- getDataDir
  createDirectoryIfMissing True "fragnix/cbits/"
  cFiles <- listDirectory (dataDir ++ "/fragnix/cbits/")
  for cFiles (\cFile -> do
    let source = dataDir ++ "/" ++ destination
        destination = "fragnix/cbits/" ++ cFile
    copyFile source destination)

  createDirectoryIfMissing True "fragnix/include/"
  includeFiles <- listDirectory (dataDir ++ "/fragnix/include/")
  for includeFiles (\includeFile -> do
    let source = dataDir ++ "/" ++ destination
        destination = "fragnix/include/" ++ includeFile
    copyFile source destination)

  return ()


getEnvironment :: String -> IO Environment
getEnvironment environmentName = do
  let options = defaults & header "Accept" .~ ["application/json"]
  response <- getWith options ("http://127.0.0.1:3000/environments/" ++ environmentName)
  asJSON response >>= return . constructEnvironment . view responseBody


constructEnvironment :: [(String, [Symbol])] -> Environment
constructEnvironment = Map.fromList . map (\(moduleName, symbols) ->
  (ModuleName () moduleName, symbols))



