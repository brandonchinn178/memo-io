{-# LANGUAGE CPP #-}
{- FOURMOLU_DISABLE -}

module MemoIO (
  memoIO,
#ifdef GLOBALIO_ENABLED
  globalIO,
#endif
) where

import MemoIO.Internal.Memo (memoIO)

#ifdef GLOBALIO_ENABLED
import MemoIO.Internal.Global (globalIO)
#endif
