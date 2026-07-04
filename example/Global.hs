import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.IORef (IORef, modifyIORef, readIORef)
import Data.Time (getCurrentTime)
import Data.Typeable (Typeable)
import GHC.Stack (HasCallStack)
import MemoIO (globalIO)
import qualified MemoIO.Global as Global

ref1, ref2 :: IORef Int
(ref1, ref2) = (Global.newIORef 0, Global.newIORef 0)

listRef :: (Typeable a) => IORef [a]
listRef = Global.newIORef []

main :: IO ()
main = do
  modifyIORef ref1 (+ 1)
  modifyIORef ref1 (+ 1)
  modifyIORef ref2 (+ 1)
  x1 <- readIORef ref1
  x2 <- readIORef ref2
  x1 `shouldEqual` 2
  x2 `shouldEqual` 1

  modifyIORef listRef (True :)
  modifyIORef listRef ((1 :: Int) :)
  listBools <- readIORef listRef
  listBools `shouldEqual` [True]
  listInts <- readIORef listRef
  listInts `shouldEqual` [1 :: Int]
  listStrs <- readIORef listRef
  listStrs `shouldEqual` ([] :: [String])

  let getCurrentTimeCached = globalIO getCurrentTime
  res1 <- getCurrentTimeCached
  threadDelay 100
  res2 <- getCurrentTimeCached
  res1 `shouldEqual` res2

shouldEqual :: (HasCallStack, Show a, Eq a) => a -> a -> IO ()
actual `shouldEqual` expected = unless (actual == expected) $ do
  error . unlines $
    [ "Expected: " ++ show expected
    , "Got: " ++ show actual
    ]
