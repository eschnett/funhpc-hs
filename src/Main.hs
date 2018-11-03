import Control.Concurrent
import GHC.Exts

import qualified Control.Distributed.MPI as MPI
import Control.Distributed.MPI.Server



main :: IO ()
main =
  runServer $
  do runBounce
     runTreeLocal
     runTree



runBounce :: IO ()
runBounce =
  do size <- MPI.commSize MPI.commWorld
     let msg = "Hello, World!" :: String
     putStrLn $ "Sending: " ++ show msg
     ftr <- remote (1 `mod` size) bounce msg
     msg' <- takeMVar ftr
     putStrLn $ "Received: " ++ show msg'

bounce :: String -> IO String
bounce msg =
  do rank <- MPI.commRank (lazy MPI.commWorld)
     return $ "[" ++ show rank ++ "]: " ++ msg



runTreeLocal :: IO ()
runTreeLocal =
  do let n = 100000
     putStrLn $ "Starting local tree with " ++ show n ++ " leaves..."
     n' <- ltree n
     putStrLn $ "Result: " ++ show n'

ltree :: Int -> IO Int
ltree n =
  do let n' = n - 1
     let n1 = n' `div` 2
     let n2 = n' - n1
     ftr1 <- if n1 > 0
             then local ltree n1
             else newMVar 0
     ftr2 <- if n2 > 0
             then local ltree n2
             else newMVar 0
     res1 <- takeMVar ftr1
     res2 <- takeMVar ftr2
     return $ 1 + res1 + res2



runTree :: IO ()
runTree =
  do let n = 1000
     putStrLn $ "Starting remote tree with " ++ show n ++ " leaves..."
     n' <- tree (0, n)
     putStrLn $ "Result: " ++ show n'

tree :: (Int, Int) -> IO Int
tree (r, n) =
  do size <- MPI.commSize (lazy MPI.commWorld)
     let n' = n - 1
     let n1 = n' `div` 2
     let n2 = n' - n1
     let r1 = 2 * r + 1
     let r2 = 2 * r + 2
     ftr1 <- if n1 > 0
             then remote (MPI.toRank r1 `mod` size) tree (r1, n1)
             else newMVar 0
     ftr2 <- if n2 > 0
             then remote (MPI.toRank r2 `mod` size) tree (r2, n2)
             else newMVar 0
     res1 <- takeMVar ftr1
     res2 <- takeMVar ftr2
     return $ 1 + res1 + res2
