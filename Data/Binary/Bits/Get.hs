{-# LANGUAGE RankNTypes, MagicHash, BangPatterns, CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Data.Binary.Bits.Get
-- Copyright   :  (c) Lennart Kolmodin 2010-2011
--                (c) Sylvain Henry 2015
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
            , runBitGet

            -- ** Get bytes
            , getBool
            , getWord8
            , getWord16
            , getWord32
            , getWord64
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
            , word16
            , word32
            , word64
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
import Data.Binary.Bits.BitOrder
import Data.Binary.Bits.Internal
import Data.Binary.Bits.Alignment

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

-- | Increment the current bit offset
incS :: Int -> S -> S
incS o (S bs n bo) = S (unsafeDrop d bs) n' bo
   where
      !o' = (n+o)
      !d  = byte_offset o'
      !n' = bit_offset o'

-- | Read a single bit
readBool :: S -> Bool
readBool (S bs o bo) = case bo of
   BB -> testBit (unsafeHead bs) (7-o)
   BL -> testBit (unsafeHead bs) (7-o)
   LB -> testBit (unsafeHead bs) o
   LL -> testBit (unsafeHead bs) o

-- | Extract a range of bits from (ws :: ByteString)
--
-- Constraint: 8 * (length ws -1 ) < o+n <= 8 * length ws
extract :: (Num a, FastBits a) => BitOrder -> ByteString -> Int -> Int -> a
extract bo bs o n     
   | n == 0            = 0
   | B.length bs == 0  = error "Empty ByteString"
   | otherwise         = rev . mask n . foldlWithIndex' f 0 $ bs
   where 
      -- B.foldl' with index
      foldlWithIndex' op b = fst . B.foldl' g (b,0)
         where g (b',i) w = (op b' w i, (i+1))

      -- 'or' correctly shifted words
      f b w i = b .|. (fromIntegral w `fastShift` off i)

      -- co-offset
      r = B.length bs * 8 - (o + n)

      -- shift offset depending on the byte position (0..B.length-1)
      off i = case bo of
         LB -> 8*i - o
         LL -> 8*i - o
         BB -> (B.length bs -1 - i) * 8 - r
         BL -> (B.length bs -1 - i) * 8 - r

      -- reverse bits if necessary
      rev = case bo of
         LL -> reverseBits n
         BL -> reverseBits n
         BB -> id
         LB -> id


-- | Generic readWord
readWord :: (Num a, FastBits a) => Int -> S -> a
readWord n (S bs o bo)
   | n == 0    = 0
   | otherwise = extract bo (unsafeTake nbytes bs) o n
   where nbytes = byte_offset (o+n+7)

-- | Check that the number of bits to read is not greater than the first parameter
{-# INLINE readWordChecked #-}
readWordChecked :: (Num a, FastBits a) => Int -> Int -> S -> a
readWordChecked m n s
   | n > m     = error $ "Tried to read more than " ++ show m ++ " bits (" ++ show n ++")"
   | otherwise = readWord n s

-- | Read the given number of bytes and return them in Big-Endian order
--
-- Examples:
--    BB: xxxABCDE FGHIJKLM NOPxxxxx -> ABCDEFGH IJKLMNOP
--    LB: LMNOPxxx DEFGHIJK xxxxxABC -> ABCDEFGH IJKLMNOP
--    BL: xxxPONML KJIHGFED CBAxxxxx -> ABCDEFGH IJKLMNOP
--    LL: EDCBAxxx MLKJIHGF xxxxxPON -> ABCDEFGH IJKLMNOP
readByteString :: Int -> S -> ByteString
readByteString n (S bs o bo) =
   let 
      bs'  = unsafeTake (n+1) bs
      bs'' = unsafeTake n bs
      rev  = B.map (reverseBits 8)
   in case (o,bo) of
      (0,BB) -> bs''
      (0,LB) -> B.reverse bs''
      (0,LL) -> rev bs''
      (0,BL) -> rev . B.reverse $ bs''
      (_,LB) -> readByteString n (S (B.reverse bs') (8-o) BB)
      (_,BL) -> rev . B.reverse $ readByteString n (S bs' o BB)
      (_,LL) -> rev . B.reverse $ readByteString n (S bs' o LB)
      (_,BB) -> unsafePerformIO $ do
         let len = n+1
         ptr <- mallocBytes len
         let f r i = do
               let
                  w  = unsafeIndex bs (len-i)
                  w' = (w `fastShiftL` o) .|. r
                  r' = w `fastShiftR` (8-o)
               poke (ptr `plusPtr` (len-i)) w'
               return r'
         foldM_ f 0 [1..len]
         unsafeInit <$> unsafePackMallocCStringLen (ptr,len)


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

instance BitOrderable BitGet where
   setBitOrder bo = do
      (S bs o _) <- getState
      putState (S bs o bo)

   getBitOrder = do
      (S _ _ bo) <- getState
      return bo

instance Alignable BitGet where
   -- | Skip the given number of bits
   skipBits n = do
      ensureBits n
      withState (incS n)

   -- | Skip bits if necessary to align to the next byte
   alignByte = do
      (S _ o _) <- getState
      when (o /= 0) $
         skipBits (8-o)



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
getWord16 :: Int -> BitGet Word16
getWord16 n = block (word16 n)

getWord16be :: Int -> BitGet Word16
getWord16be = getWord16
{-# DEPRECATED getWord16be "Use 'getWord16' instead" #-}

-- | Get @n@ bits as a 'Word32'. @n@ must be within @[0..32]@.
getWord32 :: Int -> BitGet Word32
getWord32 n = block (word32 n)

getWord32be :: Int -> BitGet Word32
getWord32be = getWord32
{-# DEPRECATED getWord32be "Use 'getWord32' instead" #-}

-- | Get @n@ bits as a 'Word64'. @n@ must be within @[0..64]@.
getWord64 :: Int -> BitGet Word64
getWord64 n = block (word64 n)

getWord64be :: Int -> BitGet Word64
getWord64be = getWord64
{-# DEPRECATED getWord64be "Use 'getWord64' instead" #-}

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
word16 :: Int -> Block Word16
word16 n = Block n (readWordChecked 16 n)

word16be :: Int -> Block Word16
word16be = word16
{-# DEPRECATED word16be "Use 'word16' instead" #-}

-- | Read @n@ bits as a 'Word32'. @n@ must be within @[0..32]@.
word32 :: Int -> Block Word32
word32 n = Block n (readWordChecked 32 n)

word32be :: Int -> Block Word32
word32be = word32
{-# DEPRECATED word32be "Use 'word32' instead" #-}

-- | Read @n@ bits as a 'Word64'. @n@ must be within @[0..64]@.
word64 :: Int -> Block Word64
word64 n = Block n (readWordChecked 64 n)

word64be :: Int -> Block Word64
word64be = word64
{-# DEPRECATED word64be "Use 'word64' instead" #-}

-- | Read @n@ bytes as a 'ByteString'.
byteString :: Int -> Block ByteString
byteString n | n > 0 = Block (n*8) (readByteString n)
             | otherwise = Block 0 (\_ -> B.empty)

