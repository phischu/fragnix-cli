module Fetch where


import FragnixServer (
  SliceTransitiveAPI)
import Fragnix.Slice (
  Slice, SliceID, writeSliceDefault, readSliceDefault)
import Fragnix.ModuleDeclarations (
  moduleSymbols)
import Fragnix.SliceCompiler (
  sliceModule)
import Fragnix.Environment (
  persistEnvironment, environmentPath)

import Language.Haskell.Exts (
  mkSrcSpan, noLoc)
import RunClientM (
  run)
import Servant.Client (
  client)

import qualified Data.Map as Map (
  empty, singleton)
import qualified Data.Set as Set (
  empty)
import Control.Monad (
  forM_)
import Data.Proxy (
  Proxy(Proxy))


fragnixFetch :: SliceID -> IO ()
fragnixFetch sliceID = do
  putStrLn "Fetching slice"
  slices <- getSlicesTransitive sliceID
  forM_ slices writeSliceDefault
  slice <- readSliceDefault sliceID
  let bogusSrcSpan = mkSrcSpan noLoc noLoc
      bogusInstanceMap = Map.singleton sliceID Set.empty
      sliceModuleAST = fmap (const bogusSrcSpan) (sliceModule bogusInstanceMap slice)
      sliceEnvironment = moduleSymbols Map.empty [sliceModuleAST]
  persistEnvironment environmentPath sliceEnvironment


getSlicesTransitive :: SliceID -> IO [Slice]
getSlicesTransitive sliceID = do
  run (client (Proxy :: Proxy SliceTransitiveAPI) sliceID)


