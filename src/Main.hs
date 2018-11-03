import Control.Concurrent
import GHC.Exts

import qualified Control.Distributed.MPI as MPI
-- import qualified Control.Distributed.MPI.Simple as MPI
import Control.Distributed.MPI.Server



main :: IO ()
main =
  runServer $
  do size <- MPI.commSize MPI.commWorld
     let msg = "Hello, World!" :: String
     putStrLn $ "Sending: " ++ show msg
     ftr <- remote (1 `mod` size) bounce msg
     msg' <- takeMVar ftr
     putStrLn $ "Received: " ++ show msg'
     let n = 1000
     putStrLn $ "Starting tree " ++ show n ++ "..."
     n' <- tree n
     putStrLn $ "Result: " ++ show n'

bounce :: String -> IO String
bounce msg =
  do rank <- MPI.commRank (lazy MPI.commWorld)
     return $ "[" ++ show rank ++ "]: " ++ msg

tree :: Int -> IO Int
tree n =
  do rank <- MPI.commRank (lazy MPI.commWorld)
     size <- MPI.commSize (lazy MPI.commWorld)
     let n' = n - 1
     let n1 = n' `div` 2
     let n2 = n' - n1
     ftr1 <- if n1 > 0
             then remote ((2*rank+1) `mod` size) tree n1
             else newMVar 0
     ftr2 <- if n2 > 0
             then remote ((2*rank+2) `mod` size) tree n2
             else newMVar 0
     res1 <- takeMVar ftr1
     res2 <- takeMVar ftr2
     return $ 1 + res1 + res2
