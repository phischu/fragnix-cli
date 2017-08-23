{-# LANGUAGE OverloadedStrings #-}
module Init where


import FragnixServer (
  EnvironmentAPI)
import Paths_fragnix_cli (
  getDataDir)
import Fragnix.Environment (
  persistEnvironment, environmentPath, builtinEnvironmentPath)

import Servant.Client (
  ClientEnv(ClientEnv), BaseUrl(BaseUrl), Scheme(Http),
  client, runClientM)
import Network.HTTP.Client (
  newManager, defaultManagerSettings)
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
import Control.Exception (
  throwIO)
import Data.Proxy (
  Proxy(Proxy))


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
  manager <- newManager defaultManagerSettings
  let clientEnv = ClientEnv manager (BaseUrl Http "localhost" 8081 "")
      query = client (Proxy :: Proxy EnvironmentAPI) environmentName
  result <- runClientM query clientEnv
  case result of
    Left err ->
      throwIO err
    Right flatEnvironment ->
      return (constructEnvironment flatEnvironment)


constructEnvironment :: [(String, [Symbol])] -> Environment
constructEnvironment = Map.fromList . map (\(moduleName, symbols) ->
  (ModuleName () moduleName, symbols))


