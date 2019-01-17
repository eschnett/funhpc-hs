{-# LANGUAGE StaticPointers #-}

import Control.Concurrent
import Data.Binary
import Type.Reflection

import Control.Distributed.Closure

import qualified Control.Distributed.MPI as MPI
import Control.Distributed.MPI.Server
import Data.Distributed.GlobalPtr
import Data.Distributed.Ref



main :: IO ()
main =
  runServer
  do runBounce
     runTreeLCall
     runTreeRCall
     runTreeLocal
     runTreeRemote



runBounce :: IO ()
runBounce =
  do size <- MPI.commSize MPI.commWorld
     let msg = "Hello, World!" :: String
     putStrLn $ "Sending: " ++ show msg
     ftr <- rcall (1 `mod` size) (bounceC msg)
     msg' <- takeMVar ftr
     putStrLn $ "Received: " ++ show msg'

bounce :: String -> IO String
bounce msg =
  do rank <- MPI.commRank MPI.commWorld
     return $ "[" ++ show rank ++ "]: " ++ msg

bounceC :: String -> Closure (IO String)
bounceC msg =
  closure (static bounce)
  `cap` cpure closureDict msg



runTreeLCall :: IO ()
runTreeLCall =
  do let n = 100000
     putStrLn $ "Starting local tree with " ++ show n ++ " leaves..."
     n' <- treeLCall n
     putStrLn $ "Result: " ++ show n'

treeLCall :: Int -> IO Int
treeLCall n =
  do let n' = n - 1
     let n1 = n' `div` 2
     let n2 = n' - n1
     ftr1 <- if n1 > 0
             then lcall (treeLCall n1)
             else newMVar 0
     ftr2 <- if n2 > 0
             then lcall (treeLCall n2)
             else newMVar 0
     res1 <- takeMVar ftr1
     res2 <- takeMVar ftr2
     return $ 1 + res1 + res2



runTreeRCall :: IO ()
runTreeRCall =
  do let n = 1000
     putStrLn $ "Starting remote tree with " ++ show n ++ " leaves..."
     n' <- treeRCall 0 n
     putStrLn $ "Result: " ++ show n'

treeRCall :: Int -> Int -> IO Int
treeRCall r n =
  do size <- MPI.commSize MPI.commWorld
     let n' = n - 1
     let n1 = n' `div` 2
     let n2 = n' - n1
     let r1 = 2 * r + 1
     let r2 = 2 * r + 2
     ftr1 <- if n1 > 0
             then rcall (MPI.toRank r1 `mod` size) (treeRCallC r1 n1)
             else newMVar 0
     ftr2 <- if n2 > 0
             then rcall (MPI.toRank r2 `mod` size) (treeRCallC r2 n2)
             else newMVar 0
     res1 <- takeMVar ftr1
     res2 <- takeMVar ftr2
     return $ 1 + res1 + res2

treeRCallC :: Int -> Int -> Closure (IO Int)
treeRCallC r n =
  closure (static treeRCall)
  `cap` cpure closureDict r
  `cap` cpure closureDict n



runTreeLocal :: IO ()
runTreeLocal =
  do let n = 100000
     putStrLn $ "Starting local tree with " ++ show n ++ " leaves..."
     n' <- treeLocal n
     putStrLn $ "Result: " ++ show n'

treeLocal :: ( Static (Typeable Int)
             , Static (Serializable (GlobalPtr (Object Int))))
          => Int -> IO Int
treeLocal n =
  do let n' = n - 1
     let n1 = n' `div` 2
     let n2 = n' - n1
     ftr1 <- if n1 > 0
             then local (treeLocal n1)
             else newMVar =<< newRef 0
     ftr2 <- if n2 > 0
             then local (treeLocal n2)
             else newMVar =<< newRef 0
     res1 <- fetchRef =<< takeMVar ftr1
     res2 <- fetchRef =<< takeMVar ftr2
     return $ 1 + res1 + res2



runTreeRemote :: IO ()
runTreeRemote =
  do let n = 1000
     putStrLn $ "Starting remote tree with " ++ show n ++ " leaves..."
     n' <- treeRemote 0 n
     putStrLn $ "Result: " ++ show n'

treeRemote :: Int -> Int -> IO Int
treeRemote r n =
  do size <- MPI.commSize MPI.commWorld
     let n' = n - 1
     let n1 = n' `div` 2
     let n2 = n' - n1
     let r1 = 2 * r + 1
     let r2 = 2 * r + 2
     ftr1 <- if n1 > 0
             then rcall (MPI.toRank r1 `mod` size) (treeRemoteC r1 n1)
             else newMVar 0
     ftr2 <- if n2 > 0
             then rcall (MPI.toRank r2 `mod` size) (treeRemoteC r2 n2)
             else newMVar 0
     res1 <- takeMVar ftr1
     res2 <- takeMVar ftr2
     return $ 1 + res1 + res2

treeRemoteC :: Int -> Int -> Closure (IO Int)
treeRemoteC r n =
  closure (static treeRemote)
  `cap` cpure closureDict r
  `cap` cpure closureDict n



--------------------------------------------------------------------------------

instance Static (Binary Int) where
  closureDict = closure (static Dict)
instance Static (Typeable Int) where
  closureDict = closure (static Dict)

instance Static (Binary String) where
  closureDict = closure (static Dict)
instance Static (Typeable String) where
  closureDict = closure (static Dict)

instance Static (Typeable (MVar Int)) where
  closureDict = closure (static Dict)
instance Static (Typeable (MVar String)) where
  closureDict = closure (static Dict)
