module SingleState.StrictState where


import           Control.Monad.State.Strict

import           Class
import           Data
import           Type


runSimulator :: Int -> [Int] -> [Int] -> [Int] -> State Data Data
runSimulator 0    _     _           _           = get
runSimulator size tList (i : iList) (o : oList) = do
  restTList <- runTimeSlot tList i o
  runSimulator (size - 1) restTList iList oList

runTimeSlot :: [Int] -> Int -> Int -> State Data [Int]
runTimeSlot (target : idx : rest) inst operand = do
  d <- get
  case inst of
    0 -> case (rem target sizeOfTarget) of -- Set
      0 -> state $ \s -> ((idx : rest), setTime operand d)
      1 -> state $ \s -> ((idx : rest), setBalance operand d)
      2 -> state $ \s -> ((idx : rest), setStatus operand d)
      3 -> state $ \s -> (rest, setEntry idx operand d)
    1 -> case (rem target sizeOfTarget) of -- Mod
      0 -> state $ \s -> ((idx : rest), modifyTime rF d)
      1 -> state $ \s -> ((idx : rest), modifyBalance rF d)
      2 -> state $ \s -> ((idx : rest), modifyStatus rF d)
      3 -> state $ \s -> (rest, modifyEntry rF idx d)
    -- 2 -> Add
    -- 3 -> Div
 where
  rF x = rem x operand
