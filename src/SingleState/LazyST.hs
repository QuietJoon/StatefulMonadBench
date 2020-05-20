module SingleState.LazyST where


import           Control.Monad.ST.Lazy
import           Data.STRef.Lazy

import           Class
import           Data
import           Type


runSimulator :: Int -> [Int] -> [Int] -> [Int] -> Data -> ST s Data
runSimulator size tList iList oList d = do
  dRef <- newSTRef d
  runSimulatorSub size tList iList oList dRef
  readSTRef dRef

runSimulatorSub :: Int -> [Int] -> [Int] -> [Int] -> STRef s Data -> ST s Data
runSimulatorSub 0    _     _           _           dRef = readSTRef dRef
runSimulatorSub size tList (i : iList) (o : oList) dRef = do
  restTList <- runTimeSlot tList i o dRef
  runSimulatorSub (size - 1) restTList iList oList dRef

runTimeSlot :: [Int] -> Int -> Int -> STRef s Data -> ST s [Int]
runTimeSlot (target : idx : rest) inst operand dRef = case inst of
  0 -> case targetInData of -- Set
    0 -> do
      modifySTRef dRef (setTime operand)
      return (idx : rest)
    1 -> do
      modifySTRef dRef (setBalance operand)
      return (idx : rest)
    2 -> do
      modifySTRef dRef (setStatus operand)
      return (idx : rest)
    3 -> do
      modifySTRef dRef (setEntry idx operand)
      return rest
  1 -> case targetInData of -- Mod
    0 -> do
      modifySTRef dRef (modifyTime rF)
      return (idx : rest)
    1 -> do
      modifySTRef dRef (modifyBalance rF)
      return (idx : rest)
    2 -> do
      modifySTRef dRef (modifyStatus rF)
      return (idx : rest)
    3 -> do
      modifySTRef dRef (modifyEntry rF idx)
      return rest
  -- 2 -> Add
  -- 3 -> Div
 where
  targetInData = rem target sizeOfTarget
  rF x = rem x operand
