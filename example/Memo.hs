import Control.Concurrent (threadDelay)
import Control.Monad (unless)
import Data.Time (getCurrentTime)
import GHC.Stack (HasCallStack)
import MemoIO (memoIO)

main :: IO ()
main = do
  getCurrentTimeCached <- memoIO getCurrentTime
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
