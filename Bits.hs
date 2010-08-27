module Bits where

import BitsGet
import BitsPut

import Data.Word

class BinaryBit a where
  putBits :: Int -> a -> BitPut ()
  getBits :: Int -> BitGet a

instance BinaryBit Bool where
  putBits _ = putBool
  getBits _ = getBool

instance BinaryBit Word8 where
  putBits = putWord8
  getBits = getWord8

instance BinaryBit Word16 where
  putBits = putWord16be
  getBits = getWord16be

instance BinaryBit Word32 where
  putBits = putWord32be
  getBits = getWord32be

instance BinaryBit Word64 where
  putBits = putWord64be
  getBits = getWord64be
