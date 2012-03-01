{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts, TupleSections #-}

module Main where

import Data.Binary ( encode, Binary(..) )
import Data.Binary.Get ( runGet )
import Data.Binary.Put ( runPut )

import qualified Data.Binary.Get as BG ( getWord8, getWord16be, getWord32be, getWord64be )
import qualified Data.Binary.Put as BP ( putWord8, putWord16be, putWord32be, putWord64be )

import Data.Binary.Bits
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Control.Applicative
import Data.Bits
import Data.Monoid
import Data.Word
import Foreign.Storable
import System.Random

import Test.Framework.Options ( TestOptions'(..) )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework.Runners.Console ( defaultMain )
import Test.Framework.Runners.Options ( RunnerOptions'(..) )
import Test.Framework ( Test, testGroup )
import Test.QuickCheck

main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Internal test functions"
      [ testProperty "prop_bitreq" prop_bitreq ]

  , testGroup "Custom test cases"
      [ testProperty "prop_composite_case" prop_composite_case ]

  , testGroup "getByteString"
      [ testProperty "prop_getByteString_negative" prop_getByteString_negative ]

  , testGroup "prop_bitput_with_get_from_binary"
      [ testProperty "Word8"  (prop_bitput_with_get_from_binary :: W [Word8]  -> Property)
      , testProperty "Word16" (prop_bitput_with_get_from_binary :: W [Word16] -> Property)
      , testProperty "Word32" (prop_bitput_with_get_from_binary :: W [Word32] -> Property)
      , testProperty "Word64" (prop_bitput_with_get_from_binary :: W [Word64] -> Property)
      ]

  , testGroup "prop_bitget_with_put_from_binary"
      [ testProperty "Word8"  (prop_bitget_with_put_from_binary :: W [Word8]  -> Property)
      , testProperty "Word16" (prop_bitget_with_put_from_binary :: W [Word16] -> Property)
      , testProperty "Word32" (prop_bitget_with_put_from_binary :: W [Word32] -> Property)
      , testProperty "Word64" (prop_bitget_with_put_from_binary :: W [Word64] -> Property)
      ]

  , testGroup "prop_compare_put_with_naive"
      [ testProperty "Word8"  (prop_compare_put_with_naive :: W [Word8]  -> Property)
      , testProperty "Word16" (prop_compare_put_with_naive :: W [Word16] -> Property)
      , testProperty "Word32" (prop_compare_put_with_naive :: W [Word32] -> Property)
      , testProperty "Word64" (prop_compare_put_with_naive :: W [Word64] -> Property)
      ]

  , testGroup "prop_compare_get_with_naive"
      [ testProperty "Word8"  (prop_compare_get_with_naive:: W [Word8]  -> Property)
      , testProperty "Word16" (prop_compare_get_with_naive:: W [Word16] -> Property)
      , testProperty "Word32" (prop_compare_get_with_naive:: W [Word32] -> Property)
      , testProperty "Word64" (prop_compare_get_with_naive:: W [Word64] -> Property)
      ]

  , testGroup "prop_put_with_bitreq"
      [ testProperty "Word8"  (prop_putget_with_bitreq :: W Word8  -> Property)
      , testProperty "Word16" (prop_putget_with_bitreq :: W Word16 -> Property)
      , testProperty "Word32" (prop_putget_with_bitreq :: W Word32 -> Property)
      , testProperty "Word64" (prop_putget_with_bitreq :: W Word64 -> Property)
      ]

  , testGroup "prop_putget_list_simple"
      [ testProperty "Bool"  (prop_putget_list_simple :: W [Bool]   -> Property)
      , testProperty "Word8" (prop_putget_list_simple :: W [Word8]  -> Property)
      , testProperty "Word16" (prop_putget_list_simple :: W [Word16] -> Property)
      , testProperty "Word32" (prop_putget_list_simple :: W [Word32] -> Property)
      , testProperty "Word64" (prop_putget_list_simple :: W [Word64] -> Property)
      ]

  , testGroup "prop_putget_applicative_with_bitreq"
      [ testProperty "Word8" (prop_putget_applicative_with_bitreq :: W [(Word8,Word8,Word8)]  -> Property)
      , testProperty "Word16" (prop_putget_applicative_with_bitreq :: W [(Word16,Word16,Word16)] -> Property)
      , testProperty "Word32" (prop_putget_applicative_with_bitreq :: W [(Word32,Word32,Word32)] -> Property)
      , testProperty "Word64" (prop_putget_applicative_with_bitreq :: W [(Word64,Word64,Word64)] -> Property)
      ]

  , testGroup "prop_putget_list_with_bitreq"
      [ testProperty "Word8"  (prop_putget_list_with_bitreq :: W [Word8]  -> Property)
      , testProperty "Word16" (prop_putget_list_with_bitreq :: W [Word16] -> Property)
      , testProperty "Word32" (prop_putget_list_with_bitreq :: W [Word32] -> Property)
      , testProperty "Word64" (prop_putget_list_with_bitreq :: W [Word64] -> Property)
      ]
  , testGroup "prop_bitget_bytestring_interspersed"
      [ testProperty "Word8"  (prop_bitget_bytestring_interspersed :: W Word8  -> [B.ByteString] -> Property)
      , testProperty "Word16" (prop_bitget_bytestring_interspersed :: W Word16 -> [B.ByteString] -> Property)
      , testProperty "Word32" (prop_bitget_bytestring_interspersed :: W Word32 -> [B.ByteString] -> Property)
      , testProperty "Word64" (prop_bitget_bytestring_interspersed :: W Word64 -> [B.ByteString] -> Property)
      ]
  ]


prop_getByteString_negative :: Int -> Property
prop_getByteString_negative n =
  n < 1 ==>
    runGet (runBitGet (getByteString n)) L.empty == B.empty

prop_putget_with_bitreq :: (BinaryBit a, Num a, Bits a, Ord a) => W a -> Property
prop_putget_with_bitreq (W w) = property $
  -- write all words with as many bits as it's required
  let p = putBits (bitreq w) w
      g = getBits (bitreq w)
      lbs = runPut (runBitPut p)
      w' = runGet (runBitGet g) lbs
  in w == w'

-- | Write a list of items. Each item is written with the maximum amount of
-- bits, i.e. 8 for Word8, 16 for Word16, etc.
prop_putget_list_simple :: (BinaryBit a, Eq a, Storable a) => W [a] -> Property
prop_putget_list_simple (W ws) = property $
  let s = sizeOf (head ws) * 8
      p = mapM_ (\v -> putBits s v) ws
      g = mapM  (const (getBits s)) ws
      lbs = runPut (runBitPut p)
      ws' = runGet (runBitGet g) lbs
  in ws == ws'

-- | Write a list of items. Each item is written with exactly as many bits
-- as required. Then read it back.
prop_putget_list_with_bitreq :: (BinaryBit a, Num a, Bits a, Ord a) => W [a] -> Property
prop_putget_list_with_bitreq (W ws) = property $
  -- write all words with as many bits as it's required
  let p = mapM_ (\v -> putBits (bitreq v) v) ws
      g = mapM getBits bitlist
      lbs = runPut (runBitPut p)
      ws' = runGet (runBitGet g) lbs
  in ws == ws'
  where
    bitlist = map bitreq ws

prop_putget_applicative_with_bitreq :: (BinaryBit a, Num a, Bits a, Ord a) => W [(a,a,a)] -> Property
prop_putget_applicative_with_bitreq (W ts) = property $
  let p = mapM_ (\(a,b,c) -> do putBits (bitreq a) a
                                putBits (bitreq b) b
                                putBits (bitreq c) c) ts
      g = mapM (\(a,b,c) -> (,,) <$> getBits a <*> getBits b <*> getBits c) bitlist
      lbs = runPut (runBitPut p)
      ts' = runGet (runBitGet g) lbs
  in ts == ts'
  where
    bitlist = map (\(a,b,c) -> (bitreq a, bitreq b, bitreq c)) ts

-- | Write bits using this library, and read them back using the binary
-- library.
prop_bitput_with_get_from_binary :: (BinaryBit a, Binary a, Storable a, Eq a) => W [a] -> Property
prop_bitput_with_get_from_binary (W ws) = property $
  let s = sizeOf (head ws) * 8
      p = mapM_ (putBits s) ws
      g = mapM (const get) ws
      lbs = runPut (runBitPut p)
      ws' = runGet g lbs
  in ws == ws'

-- | Write bits using the binary library, and read them back using this
-- library.
prop_bitget_with_put_from_binary :: (BinaryBit a, Binary a, Storable a, Eq a) => W [a] -> Property
prop_bitget_with_put_from_binary (W ws) = property $
  let s = sizeOf (head ws) * 8
      p = mapM_ put ws
      g = mapM (const (getBits s)) ws
      lbs = runPut p
      ws' = runGet (runBitGet g) lbs
  in ws == ws'

-- | Write each 'ByteString' with a variable sized value as a separator.
prop_bitget_bytestring_interspersed :: (BinaryBit a, Binary a, Num a, Ord a, Bits a) => W a -> [B.ByteString] -> Property
prop_bitget_bytestring_interspersed (W ws) bss = property $
  let p = mapM_ (\bs -> putBits (bitreq ws) ws >> putByteString bs) bss
      g = mapM (\bs -> (,) <$> (getBits (bitreq ws)) <*> (getByteString (B.length bs))) bss
      lbs = runPut (runBitPut p)
      r = runGet (runBitGet g) lbs
  in map (ws,) bss == r

-- | number of bits required to write @v@
bitreq :: (Num b, Bits a, Ord a) => a -> b
bitreq v = fromIntegral . head $ [ req | (req, top) <- bittable, v <= top ]

bittable :: Bits a => [(Integer, a)]
bittable = [ (fromIntegral x, (1 `shiftL` x) - 1) | x <- [1..64] ]

prop_bitreq :: W Word64 -> Property
prop_bitreq (W w) = property $
  ( w == 0 && bitreq w == 1 )
    || bitreq w == (bitreq (w `shiftR` 1)) + 1

prop_composite_case :: Bool -> W Word16 -> Property
prop_composite_case b (W w) = w < 0x8000 ==>
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
      w' = runGet (runBitGet g) lbs
  in w == w'

prop_compare_put_with_naive :: (Bits a, BinaryBit a, Ord a) => W [a] -> Property
prop_compare_put_with_naive (W ws) = property $
  let pn = mapM_ (\v -> naive_put (bitreq v) v) ws
      p  = mapM_ (\v -> putBits   (bitreq v) v) ws
      lbs_n = runPut (runBitPut pn)
      lbs   = runPut (runBitPut p)
  in lbs_n == lbs

prop_compare_get_with_naive :: (Bits a, BinaryBit a, Ord a, Num a) => W [a] -> Property
prop_compare_get_with_naive (W ws) = property $
  let gn = mapM  (\v -> naive_get (bitreq v)) ws
      g  = mapM  (\v -> getBits   (bitreq v)) ws
      p  = mapM_ (\v -> naive_put (bitreq v) v) ws
      lbs = runPut (runBitPut p)
      rn = runGet (runBitGet gn) lbs
      r  = runGet (runBitGet g ) lbs
      -- we must help our compiler to resolve the types of 'gn' and 'g'
      types = rn == ws && r == ws
  in rn == r

-- | Write one bit at a time until the full word has been written
naive_put :: (Bits a) => Int -> a -> BitPut ()
naive_put n w = mapM_ (\b -> putBool (testBit w b)) [n-1,n-2..0]

-- | Read one bit at a time until we've reconstructed the whole word
naive_get :: (Bits a, Num a) => Int -> BitGet a
naive_get n0 =
  let loop 0 acc = return acc
      loop n acc = do
        b <- getBool
        case b of
          False -> loop (n-1) (acc `shiftL` 1)
          True  -> loop (n-1) ((acc `shiftL` 1) + 1)
  in loop n0 0

shrinker :: (Num a, Ord a, Bits a) => a -> [a]
shrinker 0 = []
shrinker w = [ w `shiftR` 1 -- try to make everything roughly half size
             ] ++ [ w' -- flip bits to zero, left->right
                  | m <- [n, n-1..1]
                  , let w' = w `clearBit` m
                  , w /= w'
                  ] ++ [w-1] -- just make it a little smaller
  where
    n = bitreq w

data W a = W { unW :: a } deriving (Show, Eq, Ord)

arbitraryW :: (Arbitrary (W a)) => Gen a
arbitraryW = unW <$> arbitrary

shrinkW :: (Arbitrary (W a)) => a -> [a]
shrinkW x = unW <$> shrink (W x)

instance Arbitrary (W Bool) where
    arbitrary       = W <$> arbitrary
    shrink          = map W <$> shrink . unW

instance Arbitrary (W Word8) where
    arbitrary       = W <$> choose (minBound, maxBound)
    shrink          = map W . shrinker . unW

instance Arbitrary (W Word16) where
    arbitrary       = W <$> choose (minBound, maxBound)
    shrink          = map W . shrinker . unW

instance Arbitrary (W Word32) where
    arbitrary       = W <$> choose (minBound, maxBound)
    shrink          = map W . shrinker . unW

instance Arbitrary (W Word64) where
    arbitrary       = W <$> choose (minBound, maxBound)
    shrink          = map W . shrinker . unW

instance Arbitrary B.ByteString where
    arbitrary       = B.pack <$> arbitrary
    shrink bs       = B.pack <$> shrink (B.unpack bs)

instance (Arbitrary (W a)) => Arbitrary (W [a]) where
    arbitrary       = W . map unW <$> arbitrary
    shrink          = map (W . map unW) <$> mapM shrink . map W . unW

instance (Arbitrary (W a), Arbitrary (W b)) => Arbitrary (W (a,b)) where
    arbitrary        = (W .) . (,) <$> arbitraryW <*> arbitraryW
    shrink (W (a,b)) = (W .) . (,) <$> shrinkW a <*> shrinkW b

instance (Arbitrary (W a), Arbitrary (W b), Arbitrary (W c)) => Arbitrary (W (a,b,c)) where
    arbitrary          = ((W .) .) . (,,) <$> arbitraryW <*> arbitraryW <*> arbitraryW
    shrink (W (a,b,c)) = ((W .) .) . (,,) <$> shrinkW a <*> shrinkW b <*> shrinkW c

integralRandomR :: (Integral a, RandomGen g) => (a,a) -> g -> (a,g)
integralRandomR  (a,b) g = case randomR (fromIntegral a :: Integer,
                                         fromIntegral b :: Integer) g of
                            (x,g) -> (fromIntegral x, g)

{-
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
-}
