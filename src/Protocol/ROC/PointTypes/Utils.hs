{-# LANGUAGE TupleSections, OverloadedStrings, QuasiQuotes, TemplateHaskell, TypeFamilies, RecordWildCards,
             DeriveGeneric ,MultiParamTypeClasses ,FlexibleInstances  #-}

module Protocol.ROC.PointTypes.Utils where

import Data.Binary
import Data.Int
import Data.Binary.Get

getTLP :: Get [Word8]
getTLP = do
  t <- getWord8
  l <- getWord8
  p <- getWord8
  let tlplist = ([t] ++ [l] ++ [p])    
  return $ tlplist

anyButNull :: Get Bool 
anyButNull = do 
  c <- getWord8
  return $ test c 
  where 
    test :: Word8 -> Bool 
    test x = (fromIntegral x) == 1

getInt16 :: Get Int16
getInt16 = do
  x <- getWord16le
  return $ fromIntegral x



