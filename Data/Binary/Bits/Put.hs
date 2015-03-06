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

import Data.ByteString

import Control.Applicative
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
         BL -> w
         LB -> w `fastShiftR` cn
         LL -> w `fastShiftR` cn

      -- Select bits to store in the current byte.
      -- Put them in the correct order and return them in the least-significant
      -- bits of the returned value
      selectBits :: (Num a, FastBits a, Integral a) => a -> Word8
      selectBits x = fromIntegral $ case bo of
         BB ->                  mask cn $ x `fastShiftR` (n-cn)
         BL -> reverseBits cn $ mask cn $ x `fastShiftR` (n-cn)
         LB ->                  mask cn x
         LL -> reverseBits cn $ mask cn x

      -- shift left at the correct position
      shl :: Word8 -> Word8
      shl x = case bo of
         BB -> x `fastShiftL` (8-o-cn)
         BL -> x `fastShiftL` (8-o-cn)
         LB -> x `fastShiftL` o
         LL -> x `fastShiftL` o

-- | Put the @n@ lower bits of a 'Word8'.
putWord8 :: Int -> Word8 -> BitPut ()
putWord8 = putWord

-- | Put the @n@ lower bits of a 'Word16'.
putWord16be :: Int -> Word16 -> BitPut ()
putWord16be = putWord

-- | Put the @n@ lower bits of a 'Word32'.
putWord32be :: Int -> Word32 -> BitPut ()
putWord32be = putWord

-- | Put the @n@ lower bits of a 'Word64'.
putWord64be :: Int -> Word64 -> BitPut ()
putWord64be = putWord

-- | Put a 'ByteString'.
putByteString :: ByteString -> BitPut ()
putByteString bs = do
  offset <- hasOffset
  if offset
    then mapM_ (putWord8 8) (unpack bs) -- naive
    else joinPut (Put.putByteString bs)
  where
    hasOffset = BitPut $ \ s@(S _ _ o _) -> PairS (o /= 0) s

-- | Run a 'Put' inside 'BitPut'. Any partially written bytes will be flushed
-- before 'Put' executes to ensure byte alignment.
joinPut :: Put -> BitPut ()
joinPut m = BitPut $ \s0 -> PairS () $
  let (S b0 _ _ bo) = flushIncomplete s0
      b = Put.execPut m
  in (S (b0`mappend`b) 0 0 bo)

flush :: S -> S
flush s@(S b w o bo)
  | o > 8 = error "flush: offset > 8"
  | o == 8 = S (b `mappend` B.singleton w) 0 0 bo
  | otherwise = s

flushIncomplete :: S -> S
flushIncomplete s@(S b w o bo)
  | o == 0 = s
  | otherwise = (S (b `mappend` B.singleton w) 0 0 bo)

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
