{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}

import Control.Monad
import Network.HostName
import System.IO
import System.Mem

import Control.Distributed.Closure hiding (Static(..))
import Control.Distributed.Closure.Static

import qualified Control.Distributed.MPI as MPI
import Control.Distributed.MPI.Server
import Control.Distributed.MPI.World
import Data.Distributed.Future
import Data.Distributed.Ref



main :: IO ()
main =
  do hSetBuffering stdout LineBuffering
     runServer
       do runBounce
          runHostNames
          runTreeLCall
          runTreeRCall
          runSimpleRef
          runSimpleLocal
          runTreeLocal
          runSimpleRemote
          runTreeRemote



runBounce :: IO ()
runBounce =
  do let msg = "Hello, World!" :: String
     putStrLn $ "Sending: " ++ show msg
     ftr <- rcall (1 `mod` worldSize) (bounceC msg)
     msg' <- readFuture ftr
     putStrLn $ "Received: " ++ show msg'

bounce :: String -> IO String
bounce msg =
  do rank <- MPI.commRank MPI.commWorld
     return $ "[" ++ show rank ++ "]: " ++ msg

bounceC :: String -> Closure (IO String)
bounceC msg = static bounce `cap` cpure closureDict msg



runHostNames :: IO ()
runHostNames =
  forM_ ([0 .. worldSize - 1] :: [MPI.Rank]) \r -> rexec r hostNameC

hostName :: IO ()
hostName = do hn <- getHostName
              putStrLn $ "[" ++ show worldRank ++ "]: " ++  hn

hostNameC :: Closure (IO ())
hostNameC = static hostName



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
             else newFuture 0
     ftr2 <- if n2 > 0
             then lcall (treeLCall n2)
             else newFuture 0
     res1 <- readFuture ftr1
     res2 <- readFuture ftr2
     return $ 1 + res1 + res2



runTreeRCall :: IO ()
runTreeRCall =
  do let n = 1000
     putStrLn $ "Starting remote tree with " ++ show n ++ " leaves..."
     n' <- treeRCall 0 n
     putStrLn $ "Result: " ++ show n'

treeRCall :: Int -> Int -> IO Int
treeRCall r n =
  do let n' = n - 1
     let n1 = n' `div` 2
     let n2 = n' - n1
     let r1 = 2 * r + 1
     let r2 = 2 * r + 2
     ftr1 <- if n1 > 0
             then rcall (MPI.toRank r1 `mod` worldSize) (treeRCallC r1 n1)
             else newFuture 0
     ftr2 <- if n2 > 0
             then rcall (MPI.toRank r2 `mod` worldSize) (treeRCallC r2 n2)
             else newFuture 0
     res1 <- readFuture ftr1
     res2 <- readFuture ftr2
     return $ 1 + res1 + res2

treeRCallC :: Int -> Int -> Closure (IO Int)
treeRCallC r n =
  static treeRCall
  `cap` cpure closureDict r
  `cap` cpure closureDict n



runSimpleRef :: IO ()
runSimpleRef =
  do putStrLn "Starting simple ref"
     let n = 42 :: Int
     rn <- newRef n
     performGC
     n' <- readFuture =<< fetchRef rn
     performGC
     putStrLn $ "Result: " ++ show n' ++ " = " ++ show n



runSimpleLocal :: IO ()
runSimpleLocal =
  do putStrLn "Starting simple local"
     let n = 42 :: Int
     rn <- local (return n)
     performGC
     n' <- readFuture =<< fetchRef =<< readFuture rn
     performGC
     putStrLn $ "Result: " ++ show n' ++ " = " ++ show n



runTreeLocal :: IO ()
runTreeLocal =
  do let n = 10000
     putStrLn $ "Starting local tree with " ++ show n ++ " leaves..."
     n' <- treeLocal n
     putStrLn $ "Result: " ++ show n'

treeLocal :: Int -> IO Int
treeLocal n =
  do let n' = n - 1
     let n1 = n' `div` 2
     let n2 = n' - n1
     performGC
     ftr1 <- if n1 > 0
             then local (treeLocal n1)
             else newFuture =<< newRef 0
     ftr2 <- if n2 > 0
             then local (treeLocal n2)
             else newFuture =<< newRef 0
     res1 <- readFuture =<< fetchRef =<< readFuture ftr1
     res2 <- readFuture =<< fetchRef =<< readFuture ftr2
     return $ 1 + res1 + res2



runSimpleRemote :: IO ()
runSimpleRemote =
  do putStrLn "Starting simple remote"
     let n = 42 :: Int
     rn <- remote worldRank $ static (return n)
     performGC
     n' <- readFuture =<< fetchRef =<< readFuture rn
     performGC
     putStrLn $ "Result: " ++ show n' ++ " = " ++ show n



runTreeRemote :: IO ()
runTreeRemote =
  do let n = 1000
     putStrLn $ "Starting remote tree with " ++ show n ++ " leaves..."
     n' <- treeRemote 0 n
     putStrLn $ "Result: " ++ show n'

treeRemote :: Int -> Int -> IO Int
treeRemote r n =
  do let n' = n - 1
     let n1 = n' `div` 2
     let n2 = n' - n1
     let r1 = 2 * r + 1
     let r2 = 2 * r + 2
     performGC
     ftr1 <- if n1 > 0
             then remote (MPI.toRank r1 `mod` worldSize) (treeRemoteC r1 n1)
             else newFuture =<< newRef 0
     performGC
     ftr2 <- if n2 > 0
             then remote (MPI.toRank r2 `mod` worldSize) (treeRemoteC r2 n2)
             else newFuture =<< newRef 0
     performGC
     res1 <- readFuture =<< fetchRef =<< readFuture ftr1
     performGC
     res2 <- readFuture =<< fetchRef =<< readFuture ftr2
     performGC
     return $ 1 + res1 + res2

treeRemoteC :: Int -> Int -> Closure (IO Int)
treeRemoteC r n =
  static treeRemote
  `cap` cpure closureDict r
  `cap` cpure closureDict n
