module Main where


import           Control.DeepSeq
import           Control.Monad.State.Lazy      as SL
import           Control.Monad.State.Strict    as SS
import           Control.Monad.ST

import qualified Data.IntMap                   as IM
import           Data.List

import           Util.Adaptor.Random.SplitMix

import           Data
import           Type
import           SingleState.Pure              as SSP
import           SingleState.ST                as SSST
import           SingleState.State             as SSS
import           SingleState.StrictState       as SSSS


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

  let iData      = Data iTime iBalance iStatus IM.empty

  let resultPure = SSP.runSimulator size targetList instList operandList iData
  let (resultState, _) = SL.runState
        (SSS.runSimulator size targetList instList operandList)
        iData
  let (resultStrictState, _) = SS.runState
        (SSSS.runSimulator size targetList instList operandList)
        iData
  let resultST =
        runST (SSST.runSimulator size targetList instList operandList iData)

  print resultPure
  print resultST
  print resultState
  print resultStrictState
