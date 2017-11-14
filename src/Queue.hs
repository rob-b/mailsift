module Queue where


import           Control.Concurrent.Async       (Async, async)
import           Control.Concurrent.STM.TBQueue (TBQueue, newTBQueueIO, readTBQueue, writeTBQueue)
import           Control.Monad                  (forever, join)
import           Control.Monad.STM              (atomically)


type Action = IO ()


make :: IO (TBQueue Action)
make = newTBQueueIO 20 


run :: TBQueue Action -> Action
run q = join (atomically $ readTBQueue q)


add :: TBQueue Action -> Action -> IO ()
add q action = atomically $ writeTBQueue q action


worker :: TBQueue Action -> IO (Async a)
worker q = async $ forever $ do
  _ <- run q
  pure ()
