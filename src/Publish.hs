module Publish where


import FragnixServer (
  SlicePublishAPI)
import Fragnix.Slice (
  Reference(OtherSlice))
import Fragnix.DeclarationSlices (
  moduleReference)
import Fragnix.SliceCompiler (
  loadSlicesTransitive)
import Fragnix.Environment (
  loadEnvironment, environmentPath)

import Language.Haskell.Exts (
  ModuleName(ModuleName), Name(Ident))
import Language.Haskell.Names (
  symbolName, symbolModule)
import RunClientM (
  run)
import Servant.Client (
  client)

import qualified Data.Map as Map (
  lookup)

import Data.Proxy (
  Proxy(Proxy))


fragnixPublish :: String -> String -> IO ()
fragnixPublish moduleName name = do
  putStrLn "Publishing transitive slices"
  environment <- loadEnvironment environmentPath
  case Map.lookup (ModuleName () moduleName) environment of
    Nothing -> do
      putStrLn ("Not found: " ++ moduleName ++ " " ++ name)
    Just moduleSymbols -> do
      case filter (\symbol -> symbolName symbol == Ident () name) moduleSymbols of
        [symbol] -> do
          case moduleReference (symbolModule symbol) of
            OtherSlice sliceID -> do
              putStrLn ("Publishing slices for " ++ show sliceID)
              slices <- loadSlicesTransitive sliceID
              run (client (Proxy :: Proxy SlicePublishAPI) slices)
            _ -> putStrLn ("Not found: " ++ moduleName ++ " " ++ name)
        _ -> putStrLn ("Not found: " ++ moduleName ++ " " ++ name)


