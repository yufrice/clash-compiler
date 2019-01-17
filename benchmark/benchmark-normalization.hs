{-# LANGUAGE PartialTypeSignatures #-}

{-# OPTIONS_GHC -Wno-orphans                 #-}
{-# OPTIONS_GHC -Wno-partial-type-signatures #-}

import           Clash.Annotations.BitRepresentation.Internal (CustomReprs)
import           Clash.Annotations.TopEntity
import           Clash.Core.TyCon
import           Clash.Core.Var
import           Clash.Driver
import           Clash.Driver.Types
import           Clash.GHC.Evaluator          (reduceConstant)
import           Clash.Primitives.Types

import           Criterion.Main

import qualified Control.Concurrent.Supply    as Supply
import           Control.DeepSeq              (NFData(rnf),rwhnf)
import           Control.Exception            (finally)
import           Data.IntMap.Strict           (IntMap)
import           Data.List                    (break, isPrefixOf)
import           System.Directory             (removeDirectoryRecursive)
import           System.Environment           (getArgs, withArgs)
import           System.FilePath              (FilePath)

import BenchmarkCommon

-- | Run benchmark the normalization process
--
-- You can provide you own test cases as commandline arguments.
--
-- All arguments from the first one starting with a '-' are given to criterion.
-- All argument before that are interpreted as test cases.
main :: IO ()
main = do
  args <- getArgs
  let (fileArgs,optionArgs) = break (isPrefixOf "-") args
      tests | null fileArgs = defaultTests
            | otherwise     = fileArgs

  tmpDir <- createTemporaryClashDirectory

  finally (do
    withArgs optionArgs (defaultMain $ fmap (benchFile tmpDir) tests)
   ) (
    removeDirectoryRecursive tmpDir
   )

benchFile :: FilePath -> FilePath -> Benchmark
benchFile tmpDir src =
  env (setupEnv tmpDir src) $
    \ ~((bindingsMap,tcm,tupTcm,_topEntities,primMap,reprs,topEntityNames,topEntity),supplyN) -> do
      bench ("normalization of " ++ src)
            (nf (fst . normalizeEntity reprs bindingsMap primMap tcm tupTcm typeTrans
                                 reduceConstant topEntityNames
                                 (opts tmpDir) supplyN :: _ -> BindingMap) topEntity)

setupEnv
  :: FilePath
  -> FilePath
  -> IO ((BindingMap, TyConMap, IntMap TyConName
         ,[(Id, Maybe TopEntity, Maybe Id)]
         ,CompiledPrimMap, CustomReprs, [Id], Id
         )
        ,Supply.Supply
        )
setupEnv tmpDir src = do
  inp <- runInputStage tmpDir src
  supplyN <- Supply.newSupply
  return (inp,supplyN)

instance NFData Supply.Supply where
  rnf = rwhnf
