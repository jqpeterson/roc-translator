{-# LANGUAGE CPP, FlexibleContexts #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Trustworthy #-}
#endif

module Protocol.ROC.RocSerialize where

import Data.Word
import Data.Binary.Put
import Data.Binary
import Control.Monad
import Foreign
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Char    (chr,ord)
import Data.List    (unfoldr)
-- And needed for the instances:
import qualified Data.ByteString as B
import qualified Data.Ratio      as R

class RocSerialize f where
  rput :: f -> Put
  
instance RocSerialize Float where 
  rput = put 
  
instance RocSerialize Bool where  
  rput = put
  
instance RocSerialize Word8 where  
  rput = put

instance RocSerialize Word16 where
  rput = putWord16le
  
instance RocSerialize Word32 where  
  rput = putWord32le

instance RocSerialize Int16 where
  rput = putWord16le.fromIntegral

instance RocSerialize Int8 where
  rput = put
  
instance RocSerialize ByteString where
  rput = putLazyByteString

runPutROC :: (RocSerialize a )=> a -> ByteString
runPutROC x = runPut (rput x)