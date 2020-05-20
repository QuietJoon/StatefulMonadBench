{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveAnyClass #-}

module Data where


import           Control.DeepSeq

import           Data.IntMap                   as IM
import           Data.Maybe

import           Class
import           Type


data Data = Data
  { time :: Time
  , balance :: Balance
  , status :: Status
  , aMap :: IntMap Datum
  } deriving (Show,Eq)

sizeOfTarget :: Int
sizeOfTarget = 4

instance NFData Data where
  rnf (Data t b s m) = rnf t `seq` rnf b `seq` rnf s `seq` rnf m

instance DataClass Data where
  getTime    = time
  getBalance = balance
  getStatus  = status
  getEntry idx = fromMaybe 0 . IM.lookup idx . aMap
  setTime newTime d = d { time = newTime }
  setBalance newBalance d = d { balance = newBalance }
  setStatus newStatus d = d { status = newStatus }
  setEntry idx aDatum d = d { aMap = IM.insert idx aDatum (aMap d) }
  modifyTime f d = d { time = f (time d) }
  modifyBalance f d = d { balance = f (balance d) }
  modifyStatus f d = d { status = f (status d) }
  modifyEntry f idx d = d { aMap = IM.adjust f idx (aMap d) }

type TData = (Time, Balance, Status, IntMap Datum)

instance DataClass TData where
  getTime (time, _, _, _) = time
  getBalance (_, balance, _, _) = balance
  getStatus (_, _, status, _) = status
  getEntry idx (_, _, _, aMap) = fromJust $ IM.lookup idx aMap
  setTime newTime (_, balance, status, aMap) = (newTime, balance, status, aMap)
  setBalance newBalance (time, _, status, aMap) =
    (time, newBalance, status, aMap)
  setStatus newStatus (time, balance, _, aMap) =
    (time, balance, newStatus, aMap)
  setEntry idx aDatum (time, balance, status, aMap) =
    (time, balance, status, IM.insert idx aDatum aMap)
  modifyTime f (time, balance, status, aMap) = (f time, balance, status, aMap)
  modifyBalance f (time, balance, status, aMap) =
    (time, f balance, status, aMap)
  modifyStatus f (time, balance, status, aMap) =
    (time, balance, f status, aMap)
  modifyEntry f idx (time, balance, status, aMap) =
    (time, balance, status, IM.adjust f idx aMap)
