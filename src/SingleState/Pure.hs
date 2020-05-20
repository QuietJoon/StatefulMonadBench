--{-# LANGUAGE RecordWildCards #-}
module SingleState.Pure where


import           Class
import           Data
import           Type


runSimulator :: Int -> [Int] -> [Int] -> [Int] -> Data -> Data
runSimulator 0    _     _           _           d = d
runSimulator size tList (i : iList) (o : oList) d = runSimulator (size - 1)
                                                                 restTList
                                                                 iList
                                                                 oList
                                                                 newData
  where (restTList, newData) = runTimeSlot tList i o d

runTimeSlot :: [Int] -> Int -> Int -> Data -> ([Int], Data)
runTimeSlot (target : idx : rest) inst operand d = case inst of
  0 -> case (rem target sizeOfTarget) of -- Set
    0 -> ((idx : rest), setTime operand d)
    1 -> ((idx : rest), setBalance operand d)
    2 -> ((idx : rest), setStatus operand d)
    3 -> (rest, setEntry idx operand d)
    --_ -> trace ("Is" ++ show (rem target 4)) $ (d,rest)
  1 -> case (rem target sizeOfTarget) of -- Mod
    0 -> ((idx : rest), modifyTime (\x -> rem x operand) d)
    1 -> ((idx : rest), modifyBalance (\x -> rem x operand) d)
    2 -> ((idx : rest), modifyStatus (\x -> rem x operand) d)
    3 -> (rest, modifyEntry (\x -> rem x operand) idx d)
    --_ -> trace ("Is" ++ show (rem target 4)) $ (d,rest)
  -- 2 -> Add
  -- 3 -> Div
