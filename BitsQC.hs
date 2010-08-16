module Main where

import Data.Binary ( encode )
import Data.Binary.Get
import Data.Binary.Put

import Data.Word
import System.Random

import Bits

import Test.QuickCheck

main = quickCheck prop_Word16be_with_offset

prop_Word16be_with_offset :: Word16 -> Property
prop_Word16be_with_offset w = w < 0x8000 ==>
  let b :*: w' :*: w'' = runGet (get (RCheck RBool not "fail" `RNextTo` RWord16be 15 `RNextTo` RWord16be 16)) lbs
  in w == w' && w == w''
  where
  lbs = runPut (putWord16be w >> putWord16be w)

instance Arbitrary Word8 where
    arbitrary       = choose (minBound, maxBound)
    shrink 0        = []
    shrink n        = [ n - 1 ]

instance Arbitrary Word16 where
    arbitrary       = choose (minBound, maxBound)
    shrink 0        = []
    shrink n        = [ n - 1000, n - 100, n - 10, n - 1 ]

instance Arbitrary Word32 where
    arbitrary       = choose (minBound, maxBound)

instance Arbitrary Word64 where
    arbitrary       = choose (minBound, maxBound)


integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x, g)

instance Random Word where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word8 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word16 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word32 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)

instance Random Word64 where
  randomR = integralRandomR
  random = randomR (minBound,maxBound)
