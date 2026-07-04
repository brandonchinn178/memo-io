{- | Functions that use 'globalIO' for defining global variables.

Intended to be imported qualified, e.g.

> import qualified MemoIO.Global as Global
-}
module MemoIO.Global (
  newIORef,
  newMVar,
  newEmptyMVar,
  newTVar,
) where

import qualified Control.Concurrent.MVar as MVar
import qualified Data.IORef as IORef
import Data.Typeable (Typeable)
import qualified GHC.Conc.Sync as TVar
import GHC.Stack (HasCallStack, withFrozenCallStack)
import MemoIO.Internal.Global (globalIO)
import System.IO.Unsafe (unsafePerformIO)

{- | Create a new IORef that can be used at the top-level.

NOINLINE is not required, but is recommended for performance.

@
myRef :: IORef Int
myRef = Global.newIORef 0
{-# NOINLINE myRef #-}
@
-}
newIORef :: (HasCallStack, Typeable a) => a -> IORef.IORef a
newIORef a = withFrozenCallStack $ unsafePerformIO $ globalIO $ IORef.newIORef a

{- | Create a new MVar that can be used at the top-level.

NOINLINE is not required, but is recommended for performance.

@
myVar :: MVar Int
myVar = Global.newMVar 0
{-# NOINLINE myVar #-}
@
-}
newMVar :: (HasCallStack, Typeable a) => a -> MVar.MVar a
newMVar a = withFrozenCallStack $ unsafePerformIO $ globalIO $ MVar.newMVar a

{- | Create a new empty MVar that can be used at the top-level.

NOINLINE is not required, but is recommended for performance.

@
myVar :: MVar Int
myVar = Global.newEmptyMVar
{-# NOINLINE myVar #-}
@
-}
newEmptyMVar :: (HasCallStack, Typeable a) => MVar.MVar a
newEmptyMVar = withFrozenCallStack $ unsafePerformIO $ globalIO $ MVar.newEmptyMVar

{- | Create a new TVar that can be used at the top-level.

NOINLINE is not required, but is recommended for performance.

@
myVar :: TVar Int
myVar = Global.newTVar 0
{-# NOINLINE myVar #-}
@
-}
newTVar :: (HasCallStack, Typeable a) => a -> TVar.TVar a
newTVar a = withFrozenCallStack $ unsafePerformIO $ globalIO $ TVar.newTVarIO a
