{-# LANGUAGE RankNTypes, MagicHash, BangPatterns, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bits.Get
-- Copyright   :  (c) Lennart Kolmodin 2010-2011
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  kolmodin@gmail.com
-- Stability   :  experimental
-- Portability :  portable (should run where the package binary runs)
--
-- Parse bits easily. Parsing can be done either in a monadic style, or more
-- efficiently, using the 'Applicative' style.
--
-- For the monadic style, write your parser as a 'BitGet' monad using the
--
--   * 'getBool'
--
--   * 'getWord8'
--
--   * 'getWord16be'
--
--   * 'getWord32be'
--
--   * 'getWord64be'
--
--   * 'getByteString'
--
-- functions and run it with 'runBitGet'.
-- 
-- For the applicative style, compose the fuctions
--
--   * 'bool'
--
--   * 'word8'
--
--   * 'word16be'
--
--   * 'word32be'
--
--   * 'word64be'
--
--   * 'byteString'
--
-- to make a 'Block'.
-- Use 'block' to turn it into the 'BitGet' monad to be able to run it with
-- 'runBitGet'.
-----------------------------------------------------------------------------

module Data.Binary.Bits.Get
            (
            -- * BitGet monad

            -- $bitget

              BitGet
            , BitOrder(..)
            , runBitGet
            , selectBitOrder

            -- ** Get bytes
            , getBool
            , getWord8
            , getWord16be
            , getWord32be
            , getWord64be

            -- ** Skip bits
            , skipBits
            , alignByte

            -- * Blocks

            -- $blocks
            , Block
            , block

            -- ** Read in Blocks
            , bool
            , word8
            , word16be
            , word32be
            , word64be
            , byteString
            , Data.Binary.Bits.Get.getByteString
            , Data.Binary.Bits.Get.getLazyByteString
            , Data.Binary.Bits.Get.isEmpty

            ) where

import Data.Binary.Get as B ( Get, getLazyByteString, isEmpty )
import Data.Binary.Get.Internal as B ( get, put, ensureN )

import Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Unsafe
import Foreign.Marshal.Alloc (mallocBytes)
import Foreign.Ptr
import Foreign.Storable (poke)
import System.IO.Unsafe (unsafePerformIO)

import Data.Bits
import Data.Word
import Control.Applicative
import Control.Monad (when,foldM_)

import Prelude as P

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word
#endif


-- $bitget
-- Parse bits using a monad.
--
-- @
--myBitParser :: 'Get' ('Word8', 'Word8')
--myBitParser = 'runGetBit' parse4by4
--
--parse4by4 :: 'BitGet' ('Word8', 'Word8')
--parse4by4 = do
--   bits <- 'getWord8' 4
--   more <- 'getWord8' 4
--   return (bits,more)
-- @

-- $blocks
-- Parse more efficiently in blocks. Each block is read with only one boundry
-- check (checking that there is enough input) as the size of the block can be
-- calculated statically. This is somewhat limiting as you cannot make the
-- parsing depend on the input being parsed.
-- 
-- @
--data IPV6Header = IPV6Header {
--     ipv6Version :: 'Word8'
--   , ipv6TrafficClass :: 'Word8'
--   , ipv6FlowLabel :: 'Word32
--   , ipv6PayloadLength :: 'Word16'
--   , ipv6NextHeader :: 'Word8'
--   , ipv6HopLimit :: 'Word8'
--   , ipv6SourceAddress :: 'ByteString'
--   , ipv6DestinationAddress :: 'ByteString'
-- }
--
-- ipv6headerblock =
--         IPV6Header '<$>' 'word8' 4
--                    '<*>' 'word8' 8
--                    '<*>' 'word32be' 24
--                    '<*>' 'word16be' 16
--                    '<*>' 'word8' 8
--                    '<*>' 'word8' 8
--                    '<*>' 'byteString' 16
--                    '<*>' 'byteString' 16
--
--ipv6Header :: 'Get' IPV6Header
--ipv6Header = 'runBitGet' ('block' ipv6headerblock)
-- @

data S = S {-# UNPACK #-} !ByteString -- Input
           {-# UNPACK #-} !Int        -- Bit offset (0-7)
                          !BitOrder   -- Bit order
          deriving (Show)

-- | Compute bit offset (equivalent to x `mod` 8 but faster)
bit_offset :: Int -> Int
bit_offset n = make_mask 3 .&. n
{-# INLINE bit_offset #-}

-- | Compute byte offset (equivalent to x `div` 8 but faster)
byte_offset :: Int -> Int
byte_offset n = n `shiftR` 3
{-# INLINE byte_offset #-}

-- | Increment the current bit offset
incS :: Int -> S -> S
incS o (S bs n bo) = S (unsafeDrop d bs) n' bo
   where
      !o' = (n+o)
      !d  = byte_offset o'
      !n' = bit_offset o'

-- | Select the bit ordering
selectBitOrderS :: BitOrder -> S -> S
selectBitOrderS bo (S bs o _) = S bs o bo

-- | make_mask 3 = 00000111
make_mask :: (Bits a, Num a) => Int -> a
make_mask n = (1 `shiftL` fromIntegral n) - 1
{-# SPECIALIZE make_mask :: Int -> Int #-}
{-# SPECIALIZE make_mask :: Int -> Word #-}
{-# SPECIALIZE make_mask :: Int -> Word8 #-}
{-# SPECIALIZE make_mask :: Int -> Word16 #-}
{-# SPECIALIZE make_mask :: Int -> Word32 #-}
{-# SPECIALIZE make_mask :: Int -> Word64 #-}

-- | Keep only the n least-significant bits of the given value
mask :: (Bits a, Num a) => Int -> a -> a
mask n v = v .&. make_mask n
{-# INLINE mask #-}

-- | Bit order
--
-- E.g. two words of 5 bits: ABCDE, VWXYZ
--    - BB: ABCDEVWX YZxxxxxx
--    - LB: XYZABCDE xxxxxxVW
--    - BL: EDCBAZYX WVxxxxxx   -- not implemented
--    - LL: XWVEDCBA xxxxxxZY   -- not implemented
data BitOrder = BB | LB deriving (Show)


-- | Read a single bit
readBool :: S -> Bool
readBool (S bs o bo) = case bo of
   BB -> testBit (unsafeHead bs) (7-o)
   LB -> testBit (unsafeHead bs) o

-- | Extract a range of bits from (ws :: ByteString)
--
-- Constraint: 8 * (length ws -1 ) < o+n <= 8 * length ws
extract :: (Num a, Bits a, FastBits a) => BitOrder -> ByteString -> Int -> Int -> a
extract bo bs o n     
   | n == 0            = 0
   | B.length bs == 0  = error "Empty ByteString"
   | otherwise         = mask n (foldlWithIndex' f 0 bs)
   where 
      -- B.foldl' with index
      foldlWithIndex' op b = fst . B.foldl' g (b,0)
         where g (b',i) w = (op b' w i, (i+1))

      -- 'or' correctly shifted words
      f b w i   = b .|. (fromIntegral w `fastShift` off i)

      -- co-offset
      r         = B.length bs * 8 - (o + n)

      -- shift offset depending on the byte position (0..B.length-1)
      off i     = case bo of
         LB -> 8*i - o
         BB -> (B.length bs -1 - i) * 8 - r


-- | Generic readWord
readWord :: (Num a, Bits a, FastBits a) => Int -> S -> a
readWord n (S bs o bo)
   | n == 0    = 0
   | o+n <= 8  = extract bo (unsafeTake 1 bs) o n
   | o+n <= 16 = extract bo (unsafeTake 2 bs) o n
   | o+n <= 24 = extract bo (unsafeTake 3 bs) o n
   | o+n <= 32 = extract bo (unsafeTake 4 bs) o n
   | o+n <= 40 = extract bo (unsafeTake 5 bs) o n
   | o+n <= 48 = extract bo (unsafeTake 6 bs) o n
   | o+n <= 56 = extract bo (unsafeTake 7 bs) o n
   | o+n <= 64 = extract bo (unsafeTake 8 bs) o n
   | otherwise = extract bo (unsafeTake ((o+n+7) `div` 8)  bs) o n

-- | Check that the number of bits to read is not greater than the first parameter
{-# INLINE readWordChecked #-}
readWordChecked :: (Num a, Bits a, FastBits a) => Int -> Int -> S -> a
readWordChecked m n s
   | n > m     = error $ "Tried to read more than " ++ show m ++ " bits (" ++ show n ++")"
   | otherwise = readWord n s

-- | Read the given number of bytes and return them in Big-Endian order
--
-- Examples:
--    BB: xxxABCDE FGHIJKLM NOPQRxxx -> ABCDEFGH IJKLMNOP QRxxxxxx
--    LB: NOPQRxxx FGHIJKLM xxxABCDE -> ABCDEFGH IJKLMNOP QRxxxxxx
readByteString :: Int -> S -> ByteString
readByteString n (S bs o bo)
  | o == 0    = unsafeTake n bs
  | otherwise = unsafePerformIO $ do
      let len = n+1
      ptr <- mallocBytes len
      case bo of
         BB -> do
            let f r i = do
                  let
                     w  = unsafeIndex bs (len-i)
                     w' = (w `fastShiftL` o) .|. r
                     r' = w `fastShiftR` (8-o)
                  poke (ptr `plusPtr` (len-i)) w'
                  return r'
            foldM_ f 0 [1..len]
            unsafeInit <$> unsafePackMallocCStringLen (ptr,len)

         LB -> do
            let f r i = do
                  let
                     w  = unsafeIndex bs (i-1)
                     w' = (w `fastShiftL` (8-o)) .|. r
                     r' = w `fastShiftR` o
                  poke (ptr `plusPtr` (len-i)) w'
                  return r'
            foldM_ f 0 [1..len]
            unsafeTail <$> unsafePackMallocCStringLen (ptr,len)

------------------------------------------------------------------------
-- | 'BitGet' is a monad, applicative and a functor. See 'runBitGet'
-- for how to run it.
newtype BitGet a = B { runState :: S -> Get (S,a) }

instance Monad BitGet where
  return x = B $ \s -> return (s,x)
  fail str = B $ \s -> putBackState s >> fail str
  (B f) >>= g = B $ \s -> do (s',a) <- f s
                             runState (g a) s'

instance Functor BitGet where
  fmap f m = m >>= \a -> return (f a)

instance Applicative BitGet where
  pure x = return x
  fm <*> m = fm >>= \f -> m >>= \v -> return (f v)

-- | Run a 'BitGet' within the Binary packages 'Get' monad. If a byte has
-- been partially consumed it will be discarded once 'runBitGet' is finished.
runBitGet :: BitGet a -> Get a
runBitGet bg = do
  s <- mkInitState
  (s',a) <- runState bg s
  putBackState s'
  return a

mkInitState :: Get S
mkInitState = do
  bs <- get
  put B.empty
  return (S bs 0 BB)

putBackState :: S -> Get ()
putBackState (S bs o _) = do
 remaining <- get
 let bs' = case o of
      0 -> bs
      _ -> unsafeDrop 1 bs
 put (bs' `B.append` remaining)

getState :: BitGet S
getState = B $ \s -> return (s,s)

putState :: S -> BitGet ()
putState s = B $ \_ -> return (s,())

withState :: (S -> S) -> BitGet ()
withState f = do
   s <- getState
   putState $! f s

-- | Make sure there are at least @n@ bits.
ensureBits :: Int -> BitGet ()
ensureBits n = do
  (S bs o bo) <- getState
  if n <= (B.length bs * 8 - o)
    then return ()
    else do let currentBits = B.length bs * 8 - o
            let byteCount   = byte_offset (n - currentBits + 7)
            B $ \_ -> do B.ensureN byteCount
                         bs' <- B.get
                         put B.empty
                         return (S (bs`append`bs') o bo, ())

-- | Skip the given number of bits
skipBits :: Int -> BitGet ()
skipBits n = do
   ensureBits n
   withState (incS n)

-- | Skip bits if necessary to align to the next byte
alignByte :: BitGet ()
alignByte = do
   (S _ o _) <- getState
   when (o /= 0) $
      skipBits (8-o)

-- | Select the bit ordering
selectBitOrder :: BitOrder -> BitGet ()
selectBitOrder = withState . selectBitOrderS

-- | Test whether all input has been consumed, i.e. there are no remaining
-- undecoded bytes.
isEmpty :: BitGet Bool
isEmpty = B $ \ (S bs o bo) -> if B.null bs
                               then B.isEmpty >>= \e -> return (S bs o bo, e)
                               else return (S bs o bo, False)

-- | Get 1 bit as a 'Bool'.
getBool :: BitGet Bool
getBool = block bool

-- | Get @n@ bits as a 'Word8'. @n@ must be within @[0..8]@.
getWord8 :: Int -> BitGet Word8
getWord8 n = block (word8 n)

-- | Get @n@ bits as a 'Word16'. @n@ must be within @[0..16]@.
getWord16be :: Int -> BitGet Word16
getWord16be n = block (word16be n)

-- | Get @n@ bits as a 'Word32'. @n@ must be within @[0..32]@.
getWord32be :: Int -> BitGet Word32
getWord32be n = block (word32be n)

-- | Get @n@ bits as a 'Word64'. @n@ must be within @[0..64]@.
getWord64be :: Int -> BitGet Word64
getWord64be n = block (word64be n)

-- | Get @n@ bytes as a 'ByteString'.
getByteString :: Int -> BitGet ByteString
getByteString n = block (byteString n)

-- | Get @n@ bytes as a lazy ByteString.
getLazyByteString :: Int -> BitGet L.ByteString
getLazyByteString n = do
  (S _ o bo) <- getState
  case o of
    0 -> B $ \s -> do
            putBackState s
            lbs <- B.getLazyByteString (fromIntegral n)
            return (S B.empty 0 bo, lbs)
    _ -> L.fromChunks . (:[]) <$> Data.Binary.Bits.Get.getByteString n





-- | A block that will be read with only one boundary check. Needs to know the
-- number of bits in advance.
data Block a = Block Int (S -> a)

instance Functor Block where
  fmap f (Block i p) = Block i (\s -> f (p s))

instance Applicative Block where
  pure a = Block 0 (\_ -> a)
  (Block i p) <*> (Block j q) = Block (i+j) (\s -> p s $ q (incS i s))
  (Block i _)  *> (Block j q) = Block (i+j) (q . incS i)
  (Block i p) <*  (Block j _) = Block (i+j) p

-- | Get a block. Will be read with one single boundry check, and
-- therefore requires a statically known number of bits.
-- Build blocks using 'bool', 'word8', 'word16be', 'word32be', 'word64be',
-- 'byteString' and 'Applicative'.
block :: Block a -> BitGet a
block (Block i p) = do
  ensureBits i
  s <- getState
  putState $! (incS i s)
  return $! p s

-- | Read a 1 bit 'Bool'.
bool :: Block Bool
bool = Block 1 readBool

-- | Read @n@ bits as a 'Word8'. @n@ must be within @[0..8]@.
word8 :: Int -> Block Word8
word8 n = Block n (readWordChecked 8 n)

-- | Read @n@ bits as a 'Word16'. @n@ must be within @[0..16]@.
word16be :: Int -> Block Word16
word16be n = Block n (readWordChecked 16 n)

-- | Read @n@ bits as a 'Word32'. @n@ must be within @[0..32]@.
word32be :: Int -> Block Word32
word32be n = Block n (readWordChecked 32 n)

-- | Read @n@ bits as a 'Word64'. @n@ must be within @[0..64]@.
word64be :: Int -> Block Word64
word64be n = Block n (readWordChecked 64 n)

-- | Read @n@ bytes as a 'ByteString'.
byteString :: Int -> Block ByteString
byteString n | n > 0 = Block (n*8) (readByteString n)
             | otherwise = Block 0 (\_ -> B.empty)


---------------------------------------------------------------------
-- Unchecked shifts, from the "binary" package

-- | Class for types supporting fast bit shifting
class FastBits a where
   fastShiftR :: a -> Int -> a
   fastShiftL :: a -> Int -> a

   {-# INLINE fastShift #-}
   fastShift :: a -> Int -> a
   fastShift x n
      | n > 0 = fastShiftL x n
      | n < 0 = fastShiftR x (negate n)
      | otherwise = x

instance FastBits Word8 where
   fastShiftR = shiftr_w8
   fastShiftL = shiftl_w8

instance FastBits Word16 where
   fastShiftR = shiftr_w16
   fastShiftL = shiftl_w16

instance FastBits Word32 where
   fastShiftR = shiftr_w32
   fastShiftL = shiftl_w32

instance FastBits Word64 where
   fastShiftR = shiftr_w64
   fastShiftL = shiftl_w64


shiftl_w8  :: Word8  -> Int -> Word8
shiftl_w16 :: Word16 -> Int -> Word16
shiftl_w32 :: Word32 -> Int -> Word32
shiftl_w64 :: Word64 -> Int -> Word64

shiftr_w8  :: Word8  -> Int -> Word8
shiftr_w16 :: Word16 -> Int -> Word16
shiftr_w32 :: Word32 -> Int -> Word32
shiftr_w64 :: Word64 -> Int -> Word64

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
shiftl_w8  (W8# w)  (I# i) = W8#  (w `uncheckedShiftL#`   i)
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
shiftl_w8  = shiftL
shiftl_w16 = shiftL
shiftl_w32 = shiftL
shiftl_w64 = shiftL

shiftr_w8 = shiftR
shiftr_w16 = shiftR
shiftr_w32 = shiftR
shiftr_w64 = shiftR
#endif
