{-# LANGUAGE OverloadedStrings #-}
module Init where


import FragnixServer (
  EnvironmentAPI, EnvironmentSlicesAPI)
import Paths_fragnix_cli (
  getDataDir)
import Fragnix.Slice (
  Slice, writeSliceDefault)
import Fragnix.Environment (
  persistEnvironment, environmentPath, builtinEnvironmentPath)

import RunClientM (
  run)
import Servant.Client (
  client)
import Language.Haskell.Names (
  Environment, Symbol)
import Language.Haskell.Exts (
  ModuleName(ModuleName))

import qualified Data.Map as Map (
  fromList)
import System.Directory (
  listDirectory, createDirectoryIfMissing, copyFile)
import Data.Traversable (
  for)
import Data.Proxy (
  Proxy(Proxy))


fragnixInit :: String -> IO ()
fragnixInit environmentName = do

  putStrLn ("Initializing environment " ++ environmentName)

  builtinEnvironment <- getEnvironment "builtin_environment"
  environment <- getEnvironment environmentName
  slices <- getEnvironmentSlices environmentName

  persistEnvironment builtinEnvironmentPath builtinEnvironment
  persistEnvironment environmentPath environment
  for slices writeSliceDefault

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
  flatEnvironment <- run (client (Proxy :: Proxy EnvironmentAPI) environmentName)
  return (constructEnvironment flatEnvironment)

getEnvironmentSlices :: String -> IO [Slice]
getEnvironmentSlices environmentName = do
  run (client (Proxy :: Proxy EnvironmentSlicesAPI) environmentName)

constructEnvironment :: [(String, [Symbol])] -> Environment
constructEnvironment = Map.fromList . map (\(moduleName, symbols) ->
  (ModuleName () moduleName, symbols))


