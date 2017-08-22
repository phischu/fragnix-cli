module Build where

import Fragnix.Declaration (
    writeDeclarations)
import Fragnix.Slice (
    writeSliceDefault)
import Fragnix.Environment (
    loadEnvironment,persistEnvironment,
    environmentPath,builtinEnvironmentPath)
import Fragnix.SliceSymbols (
    updateEnvironment,findMainSliceIDs)
import Fragnix.ModuleDeclarations (
    parse, moduleDeclarationsWithEnvironment,
    moduleSymbols)
import Fragnix.DeclarationSlices (
    declarationSlices)
import Fragnix.SliceCompiler (
    writeSliceModules, invokeGHCMain)

-- import Language.Haskell.Names (ppError)

import System.Clock (
    getTime, Clock(Monotonic), toNanoSecs, diffTimeSpec)
import qualified Data.Map as Map (union)

import Control.Monad (forM_,forM)
import System.Environment (getArgs)
import Text.Printf (printf)


-- | Take a list of module paths on the command line and compile the 'main' symbol
-- to an executable.
fragnixBuild :: [FilePath] -> IO ()
fragnixBuild modulePaths = do

    putStrLn "Loading environment ..."

    environment <- timeIt (do
        builtinEnvironment <- loadEnvironment builtinEnvironmentPath
        userEnvironment <- loadEnvironment environmentPath
        return (Map.union builtinEnvironment userEnvironment))

    putStrLn "Parsing modules ..."

    modules <- timeIt (forM modulePaths parse)

    putStrLn "Extracting declarations ..."

    let declarations = moduleDeclarationsWithEnvironment environment modules
    timeIt (writeDeclarations "fragnix/temp/declarations/declarations.json" declarations)

--    let nameErrors = moduleNameErrors environment modules
--    forM_ nameErrors (\nameError -> putStrLn ("Warning: " ++ ppError nameError))

    putStrLn "Slicing ..."

    let (slices,symbolSlices) = declarationSlices declarations
    timeIt (forM_ slices writeSliceDefault)

    putStrLn "Updating environment ..."

    let updatedEnvironment = updateEnvironment symbolSlices (moduleSymbols environment modules)
    timeIt (persistEnvironment environmentPath updatedEnvironment)

    case findMainSliceIDs symbolSlices of
        [] -> putStrLn "No main symbol in modules."
        [mainSliceID] -> do
            putStrLn ("Compiling " ++ show mainSliceID)
            putStrLn ("Generating compilation units...")
            timeIt (writeSliceModules mainSliceID)
            putStrLn ("Invoking GHC")
            _ <- timeIt (invokeGHCMain mainSliceID)
            return ()
        _ -> putStrLn "Multiple main symbols in modules."

    return ()


-- | Execute the given action and print the time it took.
timeIt :: IO a -> IO a
timeIt action = do
    timeBefore <- getTime Monotonic
    result <- action
    timeAfter <- getTime Monotonic
    let timeDifference = fromIntegral (toNanoSecs (diffTimeSpec timeBefore timeAfter)) * 1e-9 :: Double
    printf "Took %6.2fs\n" timeDifference
    return result

