{-# LANGUAGE CPP, FlexibleInstances, FlexibleContexts, TupleSections #-}

module Main ( main ) where

import Data.Binary ( Binary(..) )
import Data.Binary.Get ( runGet, runGetIncremental, pushChunks, Decoder(..) )
import Data.Binary.Put ( runPut )

import Data.Binary.Bits
import Data.Binary.Bits.Get
import Data.Binary.Bits.Put
import Data.Binary.Bits.BitOrder

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

import Control.Applicative
import Data.Bits
import Data.Word
import Foreign.Storable
import Data.Traversable (traverse)
import Data.Foldable (traverse_)

import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.Framework.Runners.Console ( defaultMain )
import Test.Framework ( Test, testGroup )
import Test.QuickCheck

main :: IO ()
main = defaultMain tests

tests :: [Test]
tests =
  [ testGroup "Internal test functions"
      [ testProperty "prop_bitreq" prop_bitreq ]

  , testGroup "Custom test cases"
      [ testProperty "prop_composite_case" prop_composite_case ]

  , testGroup "getByteString"
      [ testProperty "prop_getByteString_negative" prop_getByteString_negative 
      , testProperty "prop_putByteString_getByteString" (prop_putByteString_getByteString :: BitOrder -> B.ByteString -> Property)
      , testProperty "prop_putByteString_getByteString_many" (prop_putByteString_getByteString_many :: BitOrder -> [B.ByteString] -> Property)
      ]

  , testGroup "getLazyByteString"
      [ testProperty "getLazyByteString == getByteString"
                     prop_getLazyByteString_equal_to_ByteString
      , testProperty "getLazyByteString == getByteString (with shift)"
                     prop_getLazyByteString_equal_to_ByteString2
      ]

  , testGroup "isEmpty"
      [ testProperty "prop_isEmptyOfEmptyEmpty" prop_isEmptyOfEmptyEmpty
      , testProperty "prop_isEmptyOfNonEmptyEmpty" prop_isEmptyOfNonEmptyEmpty
      , testProperty "prop_isEmptyOfConsumedEmpty" prop_isEmptyOfConsumedEmpty
      , testProperty "prop_isEmptyOfNotConsumedNotEmpty" prop_isEmptyOfNotConsumedNotEmpty
      ]

  , testGroup "Fail"
      [ testProperty "monadic fail" prop_fail ]

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
      [ testProperty "Word8"  (prop_putget_with_bitreq :: BitOrder -> W Word8  -> Property)
      , testProperty "Word16" (prop_putget_with_bitreq :: BitOrder -> W Word16 -> Property)
      , testProperty "Word32" (prop_putget_with_bitreq :: BitOrder -> W Word32 -> Property)
      , testProperty "Word64" (prop_putget_with_bitreq :: BitOrder -> W Word64 -> Property)
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
      [ testProperty "Word8"  (prop_bitget_bytestring_interspersed :: BitOrder -> W Word8  -> [B.ByteString] -> Property)
      , testProperty "Word16" (prop_bitget_bytestring_interspersed :: BitOrder -> W Word16 -> [B.ByteString] -> Property)
      , testProperty "Word32" (prop_bitget_bytestring_interspersed :: BitOrder -> W Word32 -> [B.ByteString] -> Property)
      , testProperty "Word64" (prop_bitget_bytestring_interspersed :: BitOrder -> W Word64 -> [B.ByteString] -> Property)
      ]
  , testGroup "Simulate programs"
      [ testProperty "primitive" prop_primitive
      , testProperty "many primitives in sequence" prop_program 
      ]
  ]

prop_isEmptyOfEmptyEmpty :: Bool
prop_isEmptyOfEmptyEmpty = runGet (runBitGet isEmpty) L.empty

prop_isEmptyOfNonEmptyEmpty :: L.ByteString -> Property
prop_isEmptyOfNonEmptyEmpty bs =
  not (L.null bs) ==> not (runGet (runBitGet isEmpty) bs)

prop_isEmptyOfConsumedEmpty :: L.ByteString -> Property
prop_isEmptyOfConsumedEmpty bs =
  not (L.null bs) ==>
    runGet (runBitGet (getByteString n >> isEmpty)) bs
    where n = fromIntegral $ L.length bs

prop_isEmptyOfNotConsumedNotEmpty :: L.ByteString -> Int -> Property
prop_isEmptyOfNotConsumedNotEmpty bs n =
  (fromIntegral n) < L.length bs && not (L.null bs) ==>
    not (runGet (runBitGet (getByteString n >> isEmpty)) bs)

prop_getLazyByteString_equal_to_ByteString :: L.ByteString -> Int -> Property
prop_getLazyByteString_equal_to_ByteString bs n =
  (fromIntegral n) <= L.length bs ==>
    runGet (runBitGet (getLazyByteString (fromIntegral n))) bs ==
            (L.fromChunks . (:[]) $ runGet (runBitGet (getByteString n)) bs)

prop_getLazyByteString_equal_to_ByteString2 :: L.ByteString -> Int -> Property
prop_getLazyByteString_equal_to_ByteString2 bs n =
  (L.length bs > 1) && (fromIntegral n) < L.length bs ==>
    runGet (runBitGet (getWord8 2 >> getLazyByteString (fromIntegral n))) bs ==
            (L.fromChunks . (:[]) $ runGet (runBitGet (getWord8 2 >> getByteString n)) bs)

prop_getByteString_negative :: Int -> Property
prop_getByteString_negative n =
  n < 1 ==>
    runGet (runBitGet (getByteString n)) L.empty == B.empty

prop_putByteString_getByteString :: BitOrder -> B.ByteString -> Property
prop_putByteString_getByteString bo bs = property $ bs' == bs
   where
      n   = B.length bs
      w   = runPut (runBitPut (withBitOrder bo (putByteString bs)))
      bs' = runGet (runBitGet (withBitOrder bo (getByteString n))) w

prop_putByteString_getByteString_many :: BitOrder -> [B.ByteString] -> Property
prop_putByteString_getByteString_many bo bs = property $ bs' == bs
   where
      n   = fmap B.length bs
      w   = runPut (runBitPut (withBitOrder bo (traverse_ putByteString bs)))
      bs' = runGet (runBitGet (withBitOrder bo (traverse getByteString n))) w


prop_putget_with_bitreq :: (BinaryBit a, Num a, Bits a, Ord a) => BitOrder -> W a -> Property
prop_putget_with_bitreq bo (W w) = property $
  -- write all words with as many bits as it's required
  let p = putBits (bitreq w) w
      g = getBits (bitreq w)
      lbs = runPut (runBitPut (withBitOrder bo p))
      w' = runGet (runBitGet (withBitOrder bo g)) lbs
  in w == w'

-- | Write a list of items. Each item is written with the maximum amount of
-- bits, i.e. 8 for Word8, 16 for Word16, etc.
prop_putget_list_simple :: (BinaryBit a, Eq a, Storable a) => W [a] -> Property
prop_putget_list_simple (W ws) = property $
  let s = sizeOf (head ws) * 8
      p = mapM_ (putBits s) ws
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
prop_bitget_bytestring_interspersed :: (BinaryBit a, Binary a, Num a, Ord a, Bits a) => BitOrder -> W a -> [B.ByteString] -> Property
prop_bitget_bytestring_interspersed bo (W ws) bss = property $
  let p = mapM_ (\bs -> putBits (bitreq ws) ws >> putByteString bs) bss
      g = mapM (\bs -> (,) <$> getBits (bitreq ws) <*> getByteString (B.length bs)) bss
      lbs = runPut (runBitPut (withBitOrder bo p))
      r = runGet (runBitGet (withBitOrder bo g)) lbs
  in map (ws,) bss == r

-- | Test failing.
prop_fail :: L.ByteString -> String -> Property
prop_fail lbs errMsg0 = forAll (choose (0, 8 * L.length lbs)) $ \len ->
  let (bytes,bits) = len `divMod` 8
      expectedBytesConsumed
        | bits == 0 = bytes
        | otherwise = bytes + 1
      p = do _ <- getByteString (fromIntegral bytes)
             _ <- getBits (fromIntegral bits) :: BitGet Word8
             fail errMsg0
      r = runGetIncremental (runBitGet p) `pushChunks` lbs
  in case r of
       Fail remainingBS pos errMsg ->
         and [ L.fromChunks [remainingBS] == L.drop expectedBytesConsumed lbs
             , pos == expectedBytesConsumed
             , errMsg == errMsg0
             ]
       _ -> False

-- | number of bits required to write @v@
bitreq :: (Num b, Num a, Bits a, Ord a) => a -> b
bitreq v = fromIntegral . head $ [ req | (req, top) <- bittable, v <= top ]

bittable :: (Bits a, Num a) => [(Integer, a)]
bittable = [ (fromIntegral x, (1 `shiftL` x) - 1) | x <- [1..64] ]

prop_bitreq :: W Word64 -> Property
prop_bitreq (W w) = property $
  ( w == 0 && bitreq w == 1 )
    || bitreq w == bitreq (w `shiftR` 1) + 1

prop_composite_case :: Bool -> W Word16 -> Property
prop_composite_case b (W w) = w < 0x8000 ==>
  let p = do putBool b
             putWord16 15 w
      g = do v <- getBool
             case v of
              True -> getWord16 15
              False -> do
                msb <- getWord8 7
                lsb <- getWord8 8
                return ((fromIntegral msb `shiftL` 8) .|. fromIntegral lsb)
      lbs = runPut (runBitPut p)
      w' = runGet (runBitGet g) lbs
  in w == w'

prop_compare_put_with_naive :: (Bits a, BinaryBit a, Ord a, Num a) => W [a] -> Property
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
      _types = rn == ws && r == ws
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

instance Arbitrary L.ByteString where
    arbitrary       = L.fromChunks <$> arbitrary
    shrink bs       = L.fromChunks <$> shrink (L.toChunks bs)

instance (Arbitrary (W a)) => Arbitrary (W [a]) where
    arbitrary       = W . map unW <$> arbitrary
    shrink          = map (W . map unW) <$> mapM shrink . map W . unW

instance (Arbitrary (W a), Arbitrary (W b)) => Arbitrary (W (a,b)) where
    arbitrary        = (W .) . (,) <$> arbitraryW <*> arbitraryW
    shrink (W (a,b)) = (W .) . (,) <$> shrinkW a <*> shrinkW b

instance (Arbitrary (W a), Arbitrary (W b), Arbitrary (W c)) => Arbitrary (W (a,b,c)) where
    arbitrary          = ((W .) .) . (,,) <$> arbitraryW <*> arbitraryW <*> arbitraryW
    shrink (W (a,b,c)) = ((W .) .) . (,,) <$> shrinkW a <*> shrinkW b <*> shrinkW c

data Primitive
  = Bool Bool
  | W8  Int Word8
  | W16 Int Word16
  | W32 Int Word32
  | W64 Int Word64
  | BS  Int B.ByteString
  | LBS Int L.ByteString
  | Skip Int
  | IsEmpty
  deriving (Eq, Show)

type Program = [Primitive]

instance Arbitrary Primitive where
  arbitrary = do
    let gen c = do
          let (maxBits, _) = (\w -> (finiteBitSize w, c undefined w)) undefined
          bits <- choose (0, maxBits)
          n <- choose (0, fromIntegral (2^bits-1))
          return (c bits n)
    oneof
      [ Bool <$> arbitrary
      , gen W8
      , gen W16
      , gen W32
      , gen W64
      , Skip <$> choose (0, 3000)
      , do n <- choose (0,10)
           cs <- vector n
           return (BS n (B.pack cs))
      , do n <- choose (0,10)
           cs <- vector n
           return (LBS n (L.pack cs))
      , return IsEmpty
      ]
  shrink p =
    let snk c x = map (\x' -> c (bitreq x') x') (shrinker x) in
    case p of
      Bool b -> if b then [Bool False] else []
      W8 _ x -> snk W8 x
      W16 _ x -> snk W16 x
      W32 _ x -> snk W32 x
      W64 _ x -> snk W64 x
      Skip x -> Skip <$> shrink x
      BS _ bs -> let ws = B.unpack bs in map (\ws' -> BS (length ws') (B.pack ws')) (shrink ws)
      LBS _ lbs -> let ws = L.unpack lbs in map (\ws' -> LBS (length ws') (L.pack ws')) (shrink ws)
      IsEmpty -> []

instance Arbitrary BitOrder where
   arbitrary = elements [BB, LB, LL, BL]
   shrink LL  = [BB,LB,BL]
   shrink BL  = [BB,LB]
   shrink LB  = [BB]
   shrink BB  = []


prop_primitive :: Primitive -> Property
prop_primitive prim = property $
  let p = putPrimitive prim
      g = getPrimitive prim
      lbs = runPut (runBitPut p)
      r = runGet (runBitGet g) lbs
  in r == prim

prop_program :: Program -> Property
prop_program program = property $
  let p = mapM_ putPrimitive program
      g = verifyProgram (8 * fromIntegral (L.length lbs)) program
      lbs = runPut (runBitPut p)
      r = runGet (runBitGet g) lbs
  in r

putPrimitive :: Primitive -> BitPut ()
putPrimitive p =
  case p of
    Bool b -> putBool b
    W8 n x -> putWord8 n x
    W16 n x -> putWord16 n x
    W32 n x -> putWord32 n x
    W64 n x -> putWord64 n x
    Skip n  -> skipBits n
    BS _ bs -> putByteString bs
    LBS _ lbs -> mapM_ putByteString (L.toChunks lbs)
    IsEmpty -> return ()

getPrimitive :: Primitive -> BitGet Primitive
getPrimitive p =
  case p of
    Bool _ -> Bool <$> getBool
    W8 n _ -> W8 n <$> getWord8 n
    W16 n _ -> W16 n <$> getWord16 n
    W32 n _ -> W32 n <$> getWord32 n
    W64 n _ -> W64 n <$> getWord64 n
    Skip n  -> skipBits n >> return (Skip n)
    BS n _ -> BS n <$> getByteString n
    LBS n _ -> LBS n <$> getLazyByteString n
    IsEmpty -> isEmpty >> return IsEmpty

getPrimitiveSize :: Primitive -> Int
getPrimitiveSize p = case p of
    Bool _  -> 1
    W8 n _  -> n
    W16 n _ -> n
    W32 n _ -> n
    W64 n _ -> n
    Skip n  -> n
    BS n _  -> n*8
    LBS n _ -> n*8
    IsEmpty -> 0

verifyProgram :: Int -> Program -> BitGet Bool
verifyProgram totalLength ps0 = go 0 ps0
  where
    go _ [] = return True
    go pos (p:ps) =
      case p of
        IsEmpty -> do
          let expected = pos == totalLength
          actual <- isEmpty
          if expected == actual
            then go pos ps
            else error $ "isEmpty returned wrong value, expected "
                          ++ show expected ++ " but got " ++ show actual
        _ -> check p (getPrimitive p) >> go (pos + getPrimitiveSize p) ps
    check x g = do
      y <- g
      if x == y
        then return ()
        else error $ "Roundtrip error: Expected "
                     ++ show x ++ " but got " ++ show y

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
