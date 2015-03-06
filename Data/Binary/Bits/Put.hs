-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bits.Put
-- Copyright   :  (c) Lennart Kolmodin 2010-2011
--                (c) Sylvain Henry 2015
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  kolmodin@gmail.com
-- Stability   :  experimental
-- Portability :  portable (should run where the package binary runs)
--
-- Put bits easily.
-----------------------------------------------------------------------------

module Data.Binary.Bits.Put
          ( BitPut
          , runBitPut
          , joinPut

          -- * Data types
          -- ** Bool
          , putBool

          -- ** Words
          , putWord8
          , putWord16
          , putWord32
          , putWord64
          , putWord16be
          , putWord32be
          , putWord64be

          -- ** ByteString
          , putByteString
          )
          where

import qualified Data.Binary.Builder as B
import Data.Binary.Builder ( Builder )
import qualified Data.Binary.Put as Put
import Data.Binary.Put ( Put )
import Data.Binary.Bits.Internal
import Data.Binary.Bits.BitOrder
import Data.Binary.Bits.Alignment

import Data.ByteString as BS
import Data.ByteString.Unsafe as BS

import Control.Applicative
import Control.Monad (when)
import Data.Bits
import Data.Monoid
import Data.Word

data BitPut a = BitPut { run :: (S -> PairS a) }

data PairS a = PairS a {-# UNPACK #-} !S

data S = S !Builder !Word8 !Int !BitOrder

-- | Put a 1 bit 'Bool'.
putBool :: Bool -> BitPut ()
putBool b = putWord8 1 (if b then 0xff else 0x00)


-- | Generic putWord
putWord :: (Num a, FastBits a, Integral a) => Int -> a -> BitPut ()
putWord n w = BitPut $ \s -> PairS () (putWordS n w s)

putWordS :: (Num a, FastBits a, Integral a) => Int -> a -> S -> S
putWordS n w s@(S builder b o bo) = s'
   where
      -- number of bits that will be stored in the current byte
      cn = min (8-o) n

      -- new state
      s' = case n of
            0 -> s
            _ -> putWordS (n-cn) w' (flush (S builder b' (o+cn) bo))
      
      -- new current byte
      b' = shl (selectBits w) .|. b

      -- Word containing the remaining (n-cn) bits to store in its LSB
      w' = case bo of
         BB -> w
         BL -> w `fastShiftR` cn
         LL -> w `fastShiftR` cn
         LB -> w

      -- Select bits to store in the current byte.
      -- Put them in the correct order and return them in the least-significant
      -- bits of the returned value
      selectBits :: (Num a, FastBits a, Integral a) => a -> Word8
      selectBits x = fromIntegral $ case bo of
         BB ->                  mask cn $ x `fastShiftR` (n-cn)
         LB -> reverseBits cn $ mask cn $ x `fastShiftR` (n-cn)
         LL ->                  mask cn x
         BL -> reverseBits cn $ mask cn x

      -- shift left at the correct position
      shl :: Word8 -> Word8
      shl x = case bo of
         BB -> x `fastShiftL` (8-o-cn)
         BL -> x `fastShiftL` (8-o-cn)
         LL -> x `fastShiftL` o
         LB -> x `fastShiftL` o

      flush s2@(S b2 w2 o2 bo2)
        | o2 == 8   = S (b2 `mappend` B.singleton w2) 0 0 bo2
        | otherwise = s2


-- | Put the @n@ lower bits of a 'Word8'.
putWord8 :: Int -> Word8 -> BitPut ()
putWord8 = putWord

-- | Put the @n@ lower bits of a 'Word16'.
putWord16 :: Int -> Word16 -> BitPut ()
putWord16 = putWord

putWord16be :: Int -> Word16 -> BitPut ()
putWord16be = putWord
{-# DEPRECATED putWord16be "Use 'putWord16' instead" #-}

-- | Put the @n@ lower bits of a 'Word32'.
putWord32 :: Int -> Word32 -> BitPut ()
putWord32 = putWord

putWord32be :: Int -> Word32 -> BitPut ()
putWord32be = putWord
{-# DEPRECATED putWord32be "Use 'putWord32' instead" #-}

-- | Put the @n@ lower bits of a 'Word64'.
putWord64 :: Int -> Word64 -> BitPut ()
putWord64 = putWord

putWord64be :: Int -> Word64 -> BitPut ()
putWord64be = putWord
{-# DEPRECATED putWord64be "Use 'putWord64' instead" #-}

-- | Put a 'ByteString'.
--
-- Examples: 3 bits are already written in the current byte
--    BB: ABCDEFGH IJKLMNOP -> xxxABCDE FGHIJKLM NOPxxxxx
--    LL: ABCDEFGH IJKLMNOP -> LMNOPxxx DEFGHIJK xxxxxABC
--    BL: ABCDEFGH IJKLMNOP -> xxxPONML KJIHGFED CBAxxxxx
--    LB: ABCDEFGH IJKLMNOP -> EDCBAxxx MLKJIHGF xxxxxPON
putByteString :: ByteString -> BitPut ()
putByteString bs = BitPut $ \s -> PairS () (putByteStringS bs s)

putByteStringS :: ByteString -> S -> S
putByteStringS bs s
   | BS.null bs = s
   | otherwise  = case s of
      (S builder b 0 BB) -> S (builder `mappend` B.fromByteString bs) b 0 BB
      (S builder b 0 LL) -> S (builder `mappend` B.fromByteString (BS.reverse bs)) b 0 LL
      (S builder b 0 LB) -> S (builder `mappend` B.fromByteString (rev bs)) b 0 LB
      (S builder b 0 BL) -> S (builder `mappend` B.fromByteString (rev (BS.reverse bs))) b 0 BL
      (S _ _ _ BB)       -> putByteStringS (BS.unsafeTail bs) (putWordS 8 (BS.unsafeHead bs) s)
      (S _ _ _ LL)       -> putByteStringS (BS.unsafeInit bs) (putWordS 8 (BS.unsafeLast bs) s)
      (S _ _ _ BL)       -> putByteStringS (BS.unsafeInit bs) (putWordS 8 (BS.unsafeLast bs) s)
      (S _ _ _ LB)       -> putByteStringS (BS.unsafeTail bs) (putWordS 8 (BS.unsafeHead bs) s)
   where
      rev    = BS.map (reverseBits 8)


-- | Run a 'Put' inside 'BitPut'. Any partially written bytes will be flushed
-- before 'Put' executes to ensure byte alignment.
--
-- Warning: this method does not take bit order into account (i.e. BB assumed)
joinPut :: Put -> BitPut ()
joinPut m = BitPut $ \s0 -> PairS () $
  let (S b0 _ _ bo) = flushIncomplete s0
      b = Put.execPut m
  in (S (b0`mappend`b) 0 0 bo)

flushIncomplete :: S -> S
flushIncomplete s@(S b w o bo)
  | o == 0 = s
  | otherwise = (S (b `mappend` B.singleton w) 0 0 bo)

getOffset :: BitPut Int
getOffset = BitPut $ \s@(S _ _ o _) -> PairS o s

-- | Run the 'BitPut' monad inside 'Put'.
runBitPut :: BitPut () -> Put.Put
runBitPut m = Put.putBuilder b
  where
  PairS _ s   = run m (S mempty 0 0 BB)
  (S b _ _ _) = flushIncomplete s

instance Functor BitPut where
  fmap f (BitPut k) = BitPut $ \s ->
    let PairS x s' = k s
    in PairS (f x) s'

instance Applicative BitPut where
  pure a = BitPut (\s -> PairS a s)
  (BitPut f) <*> (BitPut g) = BitPut $ \s ->
    let PairS a s' = f s
        PairS b s'' = g s'
    in PairS (a b) s''

instance Monad BitPut where
  m >>= k = BitPut $ \s ->
    let PairS a s'  = run m s
        PairS b s'' = run (k a) s'
    in PairS b s''
  return x = BitPut $ \s -> PairS x s

instance BitOrderable BitPut where
   setBitOrder bo = BitPut $ \(S bu b o _) -> PairS () (S bu b o bo)

   getBitOrder = BitPut $ \s@(S _ _ _ bo) -> PairS bo s

instance Alignable BitPut where
   -- | Skip the given number of bits
   skipBits n
      | n <= 64   = putWord64 n 0
      | otherwise = putWord64 64 0 >> skipBits (n-64)

   -- | Skip bits if necessary to align to the next byte
   alignByte = do
      o <- getOffset
      when (o /= 0) $
         skipBits (8-o)
