{-# LANGUAGE RankNTypes, MagicHash, BangPatterns, CPP #-}

module Data.Binary.Bits.Get
            ( BitGet
            , runBitGet

            , getBool
            , getWord8
            , getWord16be
            , getWord32be
            , getWord64be

            , bool
            , word8
            , word16be
            , word32be
            , word64be

            , block

            , Data.Binary.Bits.Get.getByteString
            ) where

import Data.Binary.Get as B ( runGet, Get, getByteString )

import Data.ByteString as B
import Data.ByteString.Internal
import Data.ByteString.Unsafe

import qualified Data.ByteString.Lazy as L

import Data.Bits
import Data.Word
import Data.List as List ( reverse )

import Control.Applicative

import Prelude as P

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
import GHC.Int
#endif

data S = S !ByteString -- ^ Input
           !Int -- ^ Bit offset (0-7)
          deriving (Show)

-- | A block that will be read with only one boundry check.
data Block a = Block Int (S -> a)

instance Functor Block where
  fmap f (Block i p) = Block i (\s -> f (p s))

instance Applicative Block where
  pure a = Block 0 (\_ -> a)
  (Block i p) <*> (Block j q) = Block (i+j) (\s -> p s $ q (incS i s))
  (Block i _)  *> (Block j q) = Block (i+j) (q . incS i)
  (Block i p) <*  (Block j _) = Block (i+j) p

block :: Block a -> BitGet a
block (Block i p) = do
  ensureBits i
  modifyState (\s -> (incS i s, p s))

-- | make_mask 3 = 00000111
make_mask :: Bits a => Int -> a
make_mask n = (1 `shiftL` fromIntegral n) - 1
{-# SPECIALIZE make_mask :: Int -> Int #-}
{-# SPECIALIZE make_mask :: Int -> Word #-}
{-# SPECIALIZE make_mask :: Int -> Word8 #-}
{-# SPECIALIZE make_mask :: Int -> Word16 #-}
{-# SPECIALIZE make_mask :: Int -> Word32 #-}
{-# SPECIALIZE make_mask :: Int -> Word64 #-}

readBool :: S -> Bool
readBool (S bs n) = testBit (unsafeHead bs) (7-n)

incS :: Int -> S -> S
incS o (S bs n) =
  let !o' = (n+o)
      !d = o' `shiftR` 3
      !n' = o' .&. make_mask 3
  in S (unsafeDrop d bs) n'

readByteString :: Int -> S -> ByteString
readByteString n s@(S bs o)
  -- no offset, easy.
  | o == 0 = unsafeTake n bs
  -- offset. ugg. this is really naive and slow. but also pretty easy :)
  | otherwise = B.pack (P.map (readWord8 8) (P.take n (iterate (incS 8) s)))

readWord8 :: Int -> S -> Word8
readWord8 n s@(S bs o)
  -- no bits at all, return 0
  | n == 0 = 0

  -- all bits are in the same byte
  -- we just need to shift and mask them right
  | n <= 8 - o = let w = unsafeHead bs
                     m = make_mask n
                     w' = (w `shiftr_w8` (8 - o - n)) .&. m
                 in w'

  -- the bits are in two different bytes
  -- make a word16 using both bytes, and then shift and mask
  | n <= 8 = let w = (fromIntegral (unsafeHead bs) `shiftl_w16` 8) .|.
                     (fromIntegral (unsafeIndex bs 1))
                 m = make_mask n
                 w' = (w `shiftr_w16` (16 - o - n)) .&. m
             in fromIntegral w'

bit_offset :: Int -> Int
bit_offset n = make_mask 3 .&. n

byte_offset :: Int -> Int
byte_offset n = n `shiftR` 3

readWord16be :: Int -> S -> Word16
readWord16be n s@(S bs o)

  -- 8 or fewer bits, use readWord8
  | n <= 8 = fromIntegral (readWord8 n s)

  -- handle 9 or more bits, stored in two bytes

  -- no offset, plain and simple 16 bytes
  | o == 0 && n == 16 = let msb = fromIntegral (unsafeHead bs)
                            lsb = fromIntegral (unsafeIndex bs 1)
                            w = (msb `shiftl_w16` 8) .|. lsb
                        in w

  -- no offset, but not full 16 bytes
  | o == 0 = let msb = fromIntegral (unsafeHead bs)
                 lsb = fromIntegral (unsafeIndex bs 1)
                 w = (msb `shiftl_w16` (n-8)) .|. (lsb `shiftr_w16` (16-n))
             in w

  -- with offset, and n=9-16
  | n <= 16 = readWithOffset s shiftl_w16 shiftr_w16 n

  | otherwise = error "readWord16be: tried to read more than 16 bits"

readWithoutOffset :: (Bits a, Num a)
                  => S -> (a -> Int -> a) -> (a -> Int -> a) -> Int -> a
readWithoutOffset s@(S bs o) shifterL shifterR n
  | o /= 0 = error "readWithoutOffset: there is an offset"

  | bit_offset n == 0 && byte_offset n <= 4 = 
              let segs = byte_offset n
                  bn 0 = fromIntegral (unsafeHead bs)
                  bn n = (bn (n-1) `shifterL` 8) .|. fromIntegral (unsafeIndex bs n)

              in bn (segs-1)

  | n <= 64 = let segs = byte_offset n
                  o' = bit_offset (n - 8 + o)

                  bn 0 = fromIntegral (unsafeHead bs)
                  bn n = (bn (n-1) `shifterL` 8) .|. fromIntegral (unsafeIndex bs n)

                  msegs = bn (segs-1) `shifterL` o'

                  last = (fromIntegral (unsafeIndex bs segs)) `shifterR` (8 - o')

                  w = msegs .|. last
              in w

readWithOffset :: (Bits a, Num a)
	       => S -> (a -> Int -> a) -> (a -> Int -> a) -> Int -> a
readWithOffset s@(S bs o) shifterL shifterR n
  | n <= 64 = let bits_in_msb = 8 - o
                  (n',top) = (n - bits_in_msb
                             , (fromIntegral (unsafeHead bs) .&. make_mask bits_in_msb) `shifterL` n')
                    
                  segs = byte_offset n'

                  bn 0 = 0
                  bn n = (bn (n-1) `shifterL` 8) .|. fromIntegral (unsafeIndex bs n)

                  o' = bit_offset n'

                  mseg = bn segs `shifterL` o'

                  last | o' > 0 = (fromIntegral (unsafeIndex bs (segs + 1))) `shifterR` (8 - o')
                       | otherwise = 0

                  w = top .|. mseg .|. last
              in w


readWord32be :: Int -> S -> Word32
readWord32be n s@(S bs o)
  -- 8 or fewer bits, use readWord8
  | n <= 8 = fromIntegral (readWord8 n s)

  -- 16 or fewer bits, use readWord16be
  | n <= 16 = fromIntegral (readWord16be n s)

  | o == 0 = readWithoutOffset s shiftl_w32 shiftr_w32 n

  | n <= 32 = readWithOffset s shiftl_w32 shiftr_w32 n

  | otherwise = error "readWord32be: tried to read more than 32 bits"


readWord64be :: Int -> S -> Word64
readWord64be n s@(S bs o)
  -- 8 or fewer bits, use readWord8
  | n <= 8 = fromIntegral (readWord8 n s)

  -- 16 or fewer bits, use readWord16be
  | n <= 16 = fromIntegral (readWord16be n s)

  | o == 0 = readWithoutOffset s shiftl_w64 shiftr_w64 n

  | n <= 64 = readWithOffset s shiftl_w64 shiftr_w64 n

  | otherwise = error "readWord64be: tried to read more than 64 bits"

------------------------------------------------------------------------
-- unrolled codensity/state monad
newtype BitGet a = B { runState :: S -> Get (S,a) }

instance Monad BitGet where
  return x = B $ \s -> return (s,x)
  fail str = B $ \_s -> fail str
  (B f) >>= g = B $ \s -> do (s',a) <- f s
                             runState (g a) s'

instance Functor BitGet where
  fmap f m = m >>= \a -> return (f a)

instance Applicative BitGet where
  pure x = return x
  fm <*> m = fm >>= \f -> m >>= \v -> return (f v)

runBitGet :: BitGet a -> Get a
runBitGet bg = do
  (_,a) <- runState bg (S B.empty 0)
  return a

getState :: BitGet S
getState = B $ \s -> return (s,s)

putState :: S -> BitGet ()
putState s = B $ \_ -> return (s,())

modifyState :: (S -> (S,a)) -> BitGet a
modifyState f = B $ \s -> return (f s)

-- | Make sure there are at least @n@ bits.
ensureBits :: Int -> BitGet ()
ensureBits n = do
  s@(S bs o) <- getState
  if n <= (B.length bs * 8 - o)
    then return ()
    else do let currentBits = B.length bs * 8 - o
            let byteCount = (n - currentBits + 7) `div` 8
            B $ \s -> do bs' <- B.getByteString byteCount
                         return (S (bs`append`bs') o, ())

getBool :: BitGet Bool
getBool = block bool

getWord8 :: Int -> BitGet Word8
getWord8 n = block (word8 n)

getWord16be :: Int -> BitGet Word16
getWord16be n = block (word16be n)

getWord32be :: Int -> BitGet Word32
getWord32be n = block (word32be n)

getWord64be :: Int -> BitGet Word64
getWord64be n = block (word64be n)

getByteString :: Int -> BitGet ByteString
getByteString n = block (byteString n)

bool :: Block Bool
bool = Block 1 readBool

word8 :: Int -> Block Word8
word8 n = Block n (readWord8 n)

word16be :: Int -> Block Word16
word16be n = Block n (readWord16be n)

word32be :: Int -> Block Word32
word32be n = Block n (readWord32be n)

word64be :: Int -> Block Word64
word64be n = Block n (readWord64be n)

byteString :: Int -> Block ByteString
byteString n = Block (n*8) (readByteString n)

------------------------------------------------------------------------
-- Unchecked shifts, from the package binary

shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w8  (W8#  w) (I# i) = W8# (w `uncheckedShiftL#`   i)
shiftl_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftL#`   i)
shiftl_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftL#`   i)

shiftr_w8  (W8#  w) (I# i) = W8# (w `uncheckedShiftRL#`   i)
shiftr_w16 (W16# w) (I# i) = W16# (w `uncheckedShiftRL#`  i)
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`  i)


#if WORD_SIZE_IN_BITS < 64
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL64#`  i)
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL64#` i)

#if __GLASGOW_HASKELL__ <= 606
-- Exported by GHC.Word in GHC 6.8 and higher
foreign import ccall unsafe "stg_uncheckedShiftL64"
    uncheckedShiftL64#     :: Word64# -> Int# -> Word64#
foreign import ccall unsafe "stg_uncheckedShiftRL64"
    uncheckedShiftRL64#     :: Word64# -> Int# -> Word64#
#endif

#else
shiftl_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftL#`  i)
shiftr_w64 (W64# w) (I# i) = W64# (w `uncheckedShiftRL#` i)
#endif

#else
shiftl_w8 = shiftL
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL

shiftr_w8 = shiftR
shiftr_w16 = shiftR
shiftr_w32 = shiftR
shiftr_w64 = shiftR
#endif
