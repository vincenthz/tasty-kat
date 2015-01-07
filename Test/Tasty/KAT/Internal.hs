{-# LANGUAGE CPP #-}
-- |
-- Module      : Test.Tasty.KAT.Internal
-- License     : MIT
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : Good
--
-- compat
--
module Test.Tasty.KAT.Internal
    ( unsafeCreateUptoN
    ) where

import qualified Data.ByteString.Internal as B
import Data.ByteString (ByteString)
import Foreign.Ptr
import Data.Word

#if !(MIN_VERSION_bytestring(0,10,0))
import Foreign.ForeignPtr
#endif

unsafeCreateUptoN :: Int -> (Ptr Word8 -> IO Int) -> ByteString
#if MIN_VERSION_bytestring(0,10,0)
unsafeCreateUptoN = B.unsafeCreateUptoN
#else
unsafeCreateUptoN len f = unsafePerformIO $ do
    fp <- B.mallocByteString len
    l' <- withForeignPtr fp f
    return $! B.PS fp 0 l'
#endif
