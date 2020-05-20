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

  let ssBench =
        bgroup "SingleState Simulation"
          $ [ bench "SingleState.StrictPure" $ nf
              (SSSP.runSimulator size targetList instList operandList)
              iData
            , bench "SingleState.StrictState" $ nf
              ( SS.runState
              $ SSSS.runSimulator size targetList instList operandList
              )
              iData
            , bench "SingleState.ST"
              $ nf (runST' size targetList instList operandList) iData
            , bench "SingleState.Pure"
              $ nf (SSP.runSimulator size targetList instList operandList) iData
            , bench "SingleState.State" $ nf
              ( LS.runState
              $ SSS.runSimulator size targetList instList operandList
              )
              iData
            , bench "SingleState.LazyST"
              $ nf (runLazyST' size targetList instList operandList) iData
            ]
  putStrLn "Do bench"
  defaultMainWith myConfig60s [ssBench]

runST' :: Int -> [Int] -> [Int] -> [Int] -> Data -> Data
runST' size targetList instList operandList aData =
  SST.runST (SSST.runSimulator size targetList instList operandList aData)

runLazyST' :: Int -> [Int] -> [Int] -> [Int] -> Data -> Data
runLazyST' size targetList instList operandList aData =
  LST.runST (SSLST.runSimulator size targetList instList operandList aData)
