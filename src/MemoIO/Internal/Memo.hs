module MemoIO.Internal.Memo where

import Control.Exception (mask, onException)
import qualified GHC.Conc.Sync as STM
import System.IO.Unsafe (unsafeInterleaveIO)

{- | Return an IO action that will memoize the result of the given action for
all subsequent calls.

Thread-safe, must be called in IO.
-}
memoIO :: IO a -> IO (IO a)
memoIO action = do
  var <- newMemoVar
  pure (runOnce var action)

runOnce :: MemoVar a -> IO a -> IO a
runOnce var action = mask $ \restore -> do
  result <-
    STM.atomically $ do
      memoResult <- STM.readTVar var
      case memoResult of
        MemoResult a -> pure $ Just a
        MemoInProgress -> STM.retry
        MemoNotStarted -> do
          STM.writeTVar var MemoInProgress
          pure Nothing
  case result of
    Just a -> pure a
    Nothing -> (`onException` STM.atomically (STM.writeTVar var MemoNotStarted)) $ do
      a <- restore action
      STM.atomically (STM.writeTVar var (MemoResult a))
      pure a

type MemoVar a = STM.TVar (MemoResult a)

newMemoVar :: IO (MemoVar a)
newMemoVar =
  unsafeInterleaveIO $ -- Don't pay for creating the TVar until it's actually needed
    STM.atomically (STM.newTVar MemoNotStarted)

data MemoResult a
  = MemoResult a
  | MemoInProgress
  | MemoNotStarted
