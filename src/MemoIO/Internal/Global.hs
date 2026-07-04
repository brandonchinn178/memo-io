{-# LANGUAGE CPP #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module MemoIO.Internal.Global where

import Data.Dynamic (Dynamic, dynTypeRep, fromDynamic, toDyn)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable (..))
import Data.IORef (IORef, atomicModifyIORef', newIORef)
import Data.Typeable (Typeable)
import GHC.Stack (
  HasCallStack,
  SrcLoc (..),
  callStack,
  getCallStack,
  withFrozenCallStack,
 )
import MemoIO.Internal.Memo (MemoVar, newMemoVar, runOnce)
import System.IO.Unsafe (unsafePerformIO)
import Type.Reflection (
  TypeRep,
  eqTypeRep,
  typeRep,
  type (:~~:) (..),
 )

#if !MIN_VERSION_base(4,15,0)
import GHC.Magic (noinline)
#else
import GHC.Exts (noinline)
#endif

{- | Ensure the given IO action runs at most once. Stores result in memory for
the lifetime of the entire program.

Thread-safe, may be used outside of IO. Safe to use with 'unsafePerformIO', will
still work even if inlined/duplicated. Slight runtime penalty every time it's
called, so prefer calling once and reusing the result if possible.
-}
globalIO :: forall a. (HasCallStack, Typeable a) => IO a -> IO a
globalIO action = withFrozenCallStack $ do
  let key = SomeCacheKey (newCacheKey :: CacheKey a)
  var0 <- newMemoVar
  var <- atomicModifyIORef' globalCacheRef $ \globalCache ->
    case HashMap.lookup key globalCache of
      Just var -> (globalCache, var)
      Nothing -> (HashMap.insert key var0 globalCache, var0)
  result <- runOnce var (toDyn <$> action)
  case fromDynamic result of
    Just a -> pure a
    Nothing ->
      -- Shouldn't happen, since TypeRep is part of cache key
      error . unlines $
        [ "globalIO unexpectedly memoized different type:"
        , "  * Got: " ++ show (dynTypeRep result)
        , "  * Expected: " ++ show (typeRep :: TypeRep a)
        ]

{----- Global cache -----}

globalCacheRef_caf :: IORef (HashMap SomeCacheKey (MemoVar Dynamic))
globalCacheRef_caf = unsafePerformIO $ newIORef HashMap.empty

globalCacheRef :: IORef (HashMap SomeCacheKey (MemoVar Dynamic))
globalCacheRef = noinline globalCacheRef_caf

{----- Cache key -----}

-- Define type alias ourselves since GHC.Stack.CallStack doesn't have Eq/Hashable instances.
type CallStack = [(String, SrcLoc)]

newtype CacheKey a = CacheKey (CallStack, TypeRep a)
  deriving (Eq)

instance Hashable (CacheKey a) where
  hashWithSalt salt (CacheKey (cs, rep)) = hashWithSalt salt (cs', rep)
   where
    cs' = [(s, fromSrcLoc loc) | (s, loc) <- cs]
    fromSrcLoc (SrcLoc pkg mod_ file startLine startCol endLine endCol) =
      (pkg, mod_, file, startLine, startCol, endLine, endCol)

newCacheKey :: forall a. (HasCallStack, Typeable a) => CacheKey a
newCacheKey = CacheKey (getCallStack callStack, typeRep :: TypeRep a)

data SomeCacheKey = forall a. SomeCacheKey (CacheKey a)

instance Eq SomeCacheKey where
  SomeCacheKey k1@(CacheKey (_, rep1)) == SomeCacheKey k2@(CacheKey (_, rep2)) =
    case eqTypeRep rep1 rep2 of
      Just HRefl -> k1 == k2
      Nothing -> False
instance Hashable SomeCacheKey where
  hashWithSalt salt (SomeCacheKey cacheKey) = hashWithSalt salt cacheKey
