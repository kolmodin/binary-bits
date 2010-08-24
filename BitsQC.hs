module Main where

import Data.Binary ( encode )
import Data.Binary.Get
import Data.Binary.Put

import Data.Bits
import Data.Word
import System.Random

import BitsGet as BG
import qualified BitsPut as BP

import Test.QuickCheck

main = do
  quickCheck prop_Word16be_with_offset
  quickCheck prop_Word16be_list
  quickCheck prop_SimpleCase
  quickCheck prop_Word32_from_2_Word16
  quickCheck prop_Word32_from_Word8_and_Word16
  quickCheck prop_Bools
  quickCheck prop_Word8_putget 

prop_SimpleCase :: Word16 -> Property
prop_SimpleCase w = w < 0x8000 ==>
  let p = RMap RBool $ \v -> case v of
                                True -> RWord16be 15
                                False -> RMapPure
                                            (RWord8 7 `RNextTo` RWord8 8)
                                            (\(msb:*:lsb)-> (fromIntegral msb `shiftL` 8) .|. fromIntegral lsb)
      w' = runGet (get p) lbs
  in w == w'
  where
  lbs = runPut (putWord16be w)

prop_Word8_putget :: [Word8] -> Property
prop_Word8_putget ws = length ws <= fromIntegral (maxBound :: Word8) ==>
  -- write all word8s with as many bits as it's required
  let p = mapM_ (\v -> BP.putWord8 (bitreq v) v) ws
      g = mapM BG.getWord8 bitlist
      lbs = runPut (BP.runBitPut p)
      Right ws' = runGet (runBitGet g) lbs
  in ws == ws'
  where
    bitlist = map bitreq ws

    -- number of bits required to write 'v'
bitreq v = fromIntegral . head $ [ req | (req, top) <- bittable, v <= top ]
bittable = [ (fromIntegral x, (1 `shiftL` x) - 1) | x <- [1..8] ]

prop_Bools :: [Bool] -> Property
prop_Bools bs = property $
  let p = sequence . replicate (length bs) $ BG.getBool
      Right bs' = runGet (BG.runBitGet p) lbs
  in bs == bs'
  where lbs = runPut $ BP.runBitPut (mapM_ BP.putBool bs)

prop_Word16be_with_offset :: Word16 -> Property
prop_Word16be_with_offset w = w < 0x8000 ==>
  let b :*: w' :*: w'' = runGet (get (RCheck RBool not "fail" `RNextTo` RWord16be 15 `RNextTo` RWord16be 16)) lbs
  in w == w' && w == w''
  where
  lbs = runPut (putWord16be w >> putWord16be w)

prop_Word16be_list :: Word8 -> [Word16] -> Property
prop_Word16be_list w ws = property $
  let p = RWord8 8 `RNextTo` RList (length ws) (RWord16be 16) :: R (T Word8 [Word16])
      w' :*: ws' = runGet (get p) lbs :: T Word8 [Word16]
  in ws == ws' && w == w'
  where
  lbs = runPut (putWord8 w >> mapM_ putWord16be ws)

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
