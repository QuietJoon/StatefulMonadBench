module Class where


import Type

class DataClass a where
  getTime :: a -> Time
  getBalance :: a -> Balance
  getStatus :: a -> Status
  getEntry :: Idx -> a -> Datum
  setTime :: Time -> a -> a
  setBalance :: Balance -> a -> a
  setStatus :: Status -> a -> a
  setEntry :: Idx -> Datum -> a -> a
  modifyTime :: (Time -> Time) -> a -> a
  modifyBalance :: (Balance -> Balance) -> a -> a
  modifyStatus :: (Status -> Status) -> a -> a
  modifyEntry :: (Datum -> Datum) -> Idx -> a -> a

class SimulationClass a
