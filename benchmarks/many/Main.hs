module Main where

import           Criterion.Main
import           Criterion.Types

import           Control.DeepSeq
import           Control.Monad.State.Lazy      as LS
import           Control.Monad.State.Strict    as SS
import           Control.Monad.ST.Lazy         as LST
import           Control.Monad.ST              as SST

import qualified Data.IntMap                   as IM
import           Data.List

import           Util.Adaptor.Random.SplitMix

import           Data
import           Type
import           SingleState.Pure              as SSP
import           SingleState.StrictPure        as SSSP
import           SingleState.State             as SSS
import           SingleState.StrictState       as SSSS
import           SingleState.StateT            as SSSTrans
import           SingleState.StrictStateT      as SSSSTrans
import           SingleState.LazyST            as SSLST
import           SingleState.ST                as SSST


myConfig60s =
  defaultConfig { timeLimit = 60.0, resamples = 10000, verbosity = Verbose }

randomInts = unfoldr (Just . bmwrInt 64)

main :: IO ()
main = do
  putStrLn "Initialize"
  let size                             = 10000
  let instSize                         = 2
  let targetSize                       = 16
  let operandSize                      = 256
  let i0Gen                            = mkGenFromInt 0
  let (targetGen, i1Gen)               = splitGen i0Gen
  let (instGen, i2Gen)                 = splitGen i1Gen
  let (operGen, iGen)                  = splitGen i2Gen
  let infTargetList = map (\x -> rem x targetSize) $ randomInts targetGen
  let infInstList = map (\x -> rem x instSize) $ randomInts instGen
  let infOperandList = map (\x -> rem x operandSize + 1) $ randomInts operGen
  let (iTime : iBalance : iStatus : _) = randomInts iGen
  let targetList                       = take (size * 2) infTargetList
  let instList                         = take size infInstList
  let operandList                      = take size infOperandList

  targetList `deepseq` instList `deepseq` operandList `deepseq` putStrLn
    "Evaluated"

  let iData = Data iTime iBalance iStatus IM.empty

  let
    ssBench =
      bgroup "SingleState Simulation"
        $ [ bench "SingleState.StrictPure"
            $ nf (SSSP.runSimulator size targetList instList operandList) iData
          , bench "SingleState.StrictPure'"
            $ nf (SSSP.runSimulator' size targetList instList operandList) iData
          , bench "SingleState.StrictPure''" $ nf
            (SSSP.runSimulator'' size targetList instList operandList)
            iData
          , bench "SingleState.StrictPure'''" $ nf
            (SSSP.runSimulator''' size targetList instList operandList)
            iData
          , bench "SingleState.StrictState" $ nf
            ( SS.runState
            $ SSSS.runSimulator size targetList instList operandList
            )
            iData
          , bench "SingleState.StrictState'" $ nf
            ( SS.runState
            $ SSSS.runSimulator' size targetList instList operandList
            )
            iData
          , bench "SingleState.StrictState''" $ nf
            ( SS.runState
            $ SSSS.runSimulator'' size targetList instList operandList
            )
            iData
          , bench "SingleState.StrictState'''" $ nf
            ( SS.runState
            $ SSSS.runSimulator''' size targetList instList operandList
            )
            iData
          ]
  putStrLn "Do bench"
  defaultMainWith myConfig60s [ssBench]
