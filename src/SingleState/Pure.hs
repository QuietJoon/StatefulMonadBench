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
  0 -> case targetInData of -- Set
    0 -> ((idx : rest), setTime operand d)
    1 -> ((idx : rest), setBalance operand d)
    2 -> ((idx : rest), setStatus operand d)
    3 -> (rest, setEntry idx operand d)
  1 -> case targetInData of -- Mod
    0 -> ((idx : rest), modifyTime rF d)
    1 -> ((idx : rest), modifyBalance rF d)
    2 -> ((idx : rest), modifyStatus rF d)
    3 -> (rest, modifyEntry rF idx d)
  -- 2 -> Add
  -- 3 -> Div
 where
  targetInData = rem target sizeOfTarget
  rF x = rem x operand
