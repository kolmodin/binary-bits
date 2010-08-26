module Main where

import Data.Binary ( encode )
import Data.Binary.Get ( runGet )
import Data.Binary.Put ( runPut )

import Bits

import Data.Bits
import Data.Word
import Foreign.Storable
import System.Random

import BitsGet
import BitsPut

import Test.QuickCheck

main = do
  quickCheck prop_Bools
  quickCheck prop_SimpleCase

  quickCheck (prop_putget_with_bitreq :: Word8  -> Property)
  quickCheck (prop_putget_with_bitreq :: Word16 -> Property)

  quickCheck (prop_putget_list_simple :: [Bool]   -> Property)
  quickCheck (prop_putget_list_simple :: [Word8]  -> Property)
  quickCheck (prop_putget_list_simple :: [Word16] -> Property)

  quickCheck (prop_putget_list_with_bitreq :: [Word8]  -> Property)
  quickCheck (prop_putget_list_with_bitreq :: [Word16] -> Property)

  -- these tests use the R structure
  --
  -- quickCheck prop_Word32_from_2_Word16
  -- quickCheck prop_Word32_from_Word8_and_Word16
 
prop_putget_with_bitreq :: (BinaryBit a, Num a, Bits a, Ord a) => a -> Property
prop_putget_with_bitreq w = property $
  -- write all words with as many bits as it's required
  let p = putBits (bitreq w) w
      g = getBits (bitreq w)
      lbs = runPut (runBitPut p)
      w' = runGet (runBitGetSimple g) lbs
  in w == w'

prop_putget_list_simple :: (BinaryBit a, Eq a, Storable a) => [a] -> Property
prop_putget_list_simple ws = property $
  let s = sizeOf (head ws) * 8
      p = mapM_ (\v -> putBits s v) ws
      g = mapM  (const (getBits s)) ws
      lbs = runPut (runBitPut p)
      ws' = runGet (runBitGetSimple g) lbs
  in ws == ws'

prop_putget_list_with_bitreq :: (BinaryBit a, Num a, Bits a, Ord a) => [a] -> Property
prop_putget_list_with_bitreq ws = property $
  -- write all words with as many bits as it's required
  let p = mapM_ (\v -> putBits (bitreq v) v) ws
      g = mapM getBits bitlist
      lbs = runPut (runBitPut p)
      ws' = runGet (runBitGetSimple g) lbs
  in ws == ws'
  where
    bitlist = map bitreq ws

-- number of bits required to write 'v'
bitreq :: (Num b, Bits a, Ord a) => a -> b
bitreq v = fromIntegral . head $ [ req | (req, top) <- bittable, v <= top ]

bittable :: Bits a => [(Integer, a)]
bittable = [ (fromIntegral x, (1 `shiftL` x) - 1) | x <- [1..64] ]

prop_Bools :: [Bool] -> Property
prop_Bools bs = property $
  let p = sequence . replicate (length bs) $ getBool
      Right bs' = runGet (runBitGet p) lbs
  in bs == bs'
  where lbs = runPut $ runBitPut (mapM_ putBool bs)

prop_SimpleCase :: Bool -> Word16 -> Property
prop_SimpleCase b w = w < 0x8000 ==>
  let p = do putBool b
             putWord16be 15 w
      g = do v <- getBool
             case v of
              True -> getWord16be 15
              False -> do
                msb <- getWord8 7
                lsb <- getWord8 8
                return ((fromIntegral msb `shiftL` 8) .|. fromIntegral lsb)
      lbs = runPut (runBitPut p)
      w' = runGet (runBitGetSimple g) lbs
  in w == w'
  where


{-
prop_Word32_from_Word8_and_Word16 :: Word8 -> Word16 -> Property
prop_Word32_from_Word8_and_Word16 w8 w16 = property $
  let p = RWord32be 24
      w' = runGet (get p) lbs
  in w0 == w'
  where
    lbs = runPut (putWord8 w8 >> putWord16be w16)
    w0 = ((fromIntegral w8) `shiftL` 16) .|. fromIntegral w16

prop_Word32_from_2_Word16 :: Word16 -> Word16 -> Property
prop_Word32_from_2_Word16 w1 w2 = property $
  let p = RWord32be 32
      w' = runGet (get p) lbs
  in w0 == w'
  where
    lbs = encode w0
    w0 = ((fromIntegral w1) `shiftL` 16) .|. fromIntegral w2
-}

instance Arbitrary Word8 where
    arbitrary       = choose (minBound, maxBound)
    shrink 0        = []
    shrink n        = [ n - 1 ]

instance Arbitrary Word16 where
    arbitrary       = choose (minBound, maxBound)
    shrink 0        = []
    shrink n        = [ n - 10000, n - 1000, n - 100, n - 1 ]

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
