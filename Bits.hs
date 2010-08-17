{-# LANGUAGE GADTs, RankNTypes, MagicHash, CPP #-}

module Bits ( R(..)
            , T(..)
            , get
            , getS
            , readBool
            , readWord8
            , readWord16be
            {-
            , runBitGet
            , BitGet
            -}

            ) where

import Data.Binary.Get

import Data.ByteString
import Data.ByteString.Internal
import Data.ByteString.Unsafe

import qualified Data.ByteString.Lazy as L

import Data.Bits
import Data.Word
import Data.List as List ( reverse )

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base hiding ( (:*:) )
import GHC.Word
import GHC.Int
#endif

data R a where
  RBool :: R Bool
  RWord8 :: Int -> R Word8
  RWord16be :: Int -> R Word16
  RWord32be :: Int -> R Word32
  RByteString :: Int -> R ByteString
  RThis :: a -> R a
  RNextTo :: R a -> R b -> R (T a b)
  RMap :: R b -> (b -> R a) -> R a
  RMapPure :: R a -> (a -> b) -> R b
  RList :: Int -> R a -> R [a]
  RCheck :: R a -> (a -> Bool) -> String -> R a

data T a b = !a :*: !b deriving (Show)

instance Show (R a) where
  show r = case r of
            RBool -> "Bool"
  
size_in_bits :: forall a. R a -- ^ The record
     -> Int -- ^ Number of bits
size_in_bits r = case r of
          RBool -> 1
          RWord8 n -> min n 8
          RWord16be n -> min n 16
          RWord32be n -> min n 16
          RByteString n -> 8 * n
          RThis _ -> 0
          RList n r -> n * size_in_bits r
          RMap r _ -> size_in_bits r
          RMapPure r _ -> size_in_bits r
          RCheck r _ _ -> size_in_bits r
          RNextTo a b -> size_in_bits a + size_in_bits b

data S = S !ByteString -- ^ Input
           !Int -- ^ Bit offset (0-7)
          deriving (Show)

size_in_bytes :: forall a. R a
              -> Int
size_in_bytes r = byte_offset (size_in_bits r + 7)

get :: R a -> Get a
get r = do
  bs <- getByteString (size_in_bytes r)
  a :*: s <- getS (S bs 0) r
  return a

getS :: S -> R a -> Get (T a S)
getS s0 r = do
  case r of
    RBool -> return (readBool s0)
    RWord8 n -> return (readWord8 s0 n)
    RWord16be n -> return (readWord16be s0 n)
    RWord32be n -> return (readWord32be s0 n)
    RByteString n -> return (readByteString s0 n)
    RThis x -> return (x :*: s0)
    RNextTo a b -> do
      t :*: s <- getS s0 a
      u :*: s' <- getS s b
      return (t:*:u:*:s')
    RMap r p -> do
      a :*: s@(S bs o) <- getS s0 r
      let codepath = p a
          required_bytes = byte_offset (size_in_bits r - o)
      bs' <- getByteString required_bytes
      getS (S (bs `append` bs') o) codepath
    RMapPure r f -> do
      a :*: s <- getS s0 r
      return (f a :*: s)
    RList n r ->
      let loop 0 s acc = return (List.reverse acc :*: s)
          loop m s acc = do
            a :*: s' <- getS s r
            loop (m-1) s' (a:acc)
      in loop n s0 []
    RCheck r c m -> do
      a :*: s <- getS s0 r
      if c a
        then return (a :*: s)
        else fail m

-- make_mask 3 = 00000111
make_mask :: Bits a => Int -> a
make_mask n = (1 `shiftL` fromIntegral n) - 1
{-# SPECIALIZE make_mask :: Int -> Word #-}
{-# SPECIALIZE make_mask :: Int -> Word8 #-}
{-# SPECIALIZE make_mask :: Int -> Word16 #-}
{-# SPECIALIZE make_mask :: Int -> Word32 #-}
{-# SPECIALIZE make_mask :: Int -> Word64 #-}

readBool :: S -> T Bool S
readBool s@(S bs n) = testBit (unsafeHead bs) (7-n) :*: incS s 1

incS :: S -> Int -> S
incS (S bs n) 1 =
  let
      n' = n + 1
      s' | n' == 8 = S (unsafeTail bs) 0
         | otherwise = S bs n'
  in s'
incS (S bs n) o =
  let (d,n') = divMod (n+o) 8
  in S (unsafeDrop d bs) n'


readByteString :: S -> Int -> T ByteString S
readByteString s@(S bs o) n
  | o == 0 = unsafeTake n bs :*: (S (unsafeDrop n bs) 0)
  | otherwise = unsafeTake n (unsafeTail bs) :*: (S (unsafeDrop (n+1) bs) 0)

readWord8 :: S -> Int -> T Word8 S
readWord8 s@(S bs o) n
  -- no bits at all, return 0
  | n == 0 = 0 :*: s

  -- all bits are in the same byte
  -- we just need to shift and mask them right
  | n <= 8 - o = let w = unsafeHead bs
                     m = make_mask n
                     w' = (w `shiftr_w8` (8 - o - n)) .&. m
                 in w' :*: incS s n

  -- the bits are in two different bytes
  -- make a word16 using both bytes, and then shift and mask
  | n <= 8 = let w = (fromIntegral (unsafeHead bs) `shiftl_w16` 8) .|.
                     (fromIntegral (unsafeIndex bs 1))
                 m = make_mask n
                 w' = (w `shiftr_w16` (16 - o - n)) .&. m
             in fromIntegral w' :*: incS s n

bit_offset :: Int -> Int
bit_offset n = make_mask 3 .&. n

byte_offset :: Int -> Int
byte_offset n = n `shiftR` 3

readWord16be :: S -> Int -> T Word16 S
readWord16be s@(S bs o) n

  -- 8 or fewer bits, use readWord8
  | n <= 8 = let w :*: s' = readWord8 s n
             in fromIntegral w :*: s'

  -- handle 9 or more bits, stored in two bytes

  -- no offset, plain and simple 16 bytes
  | o == 0 && n == 16 = let msb = fromIntegral (unsafeHead bs)
                            lsb = fromIntegral (unsafeIndex bs 1)
                            w = (msb `shiftl_w16` 8) .|. lsb
                        in w :*: incS s n

  -- no offset, but not full 16 bytes
  | o == 0 = let msb = fromIntegral (unsafeHead bs)
                 lsb = fromIntegral (unsafeIndex bs 1)
                 w = (msb `shiftl_w16` (n-8)) .|. (lsb `shiftr_w16` (16-n))
             in w :*: incS s n

  -- with offset, and n=9-16
  | n <= 16 = readWithOffset s shiftl_w16 shiftr_w16 n

  | otherwise = error "readWord16be: tried to read more than 16 bits"

readWithoutOffset s@(S bs o) shifterL shifterR n
  | o /= 0 = error "readWithoutOffset: there is an offset"

  -- | n == 8 = readWord8 s n
  -- | n == 16 = readWord16be s n
  | n == 24 || n == 32 =
              let segs = byte_offset n
                  bn 0 = fromIntegral (unsafeHead bs)
                  bn n = (bn (n-1) `shifterL` 8) .|. fromIntegral (unsafeIndex bs n)

              in (bn (segs-1) :*: incS s n)

  | n <= 64 = let segs = byte_offset n

                  bn 0 = fromIntegral (unsafeHead bs)
                  bn n = (bn (n-1) `shifterL` 8) .|. fromIntegral (unsafeIndex bs n)

                  last = (fromIntegral (unsafeIndex bs (segs + 1))) `shifterR` (8 - (bit_offset n))

                  w = bn segs .|. last
              in w :*: incS s n

readWithOffset s@(S bs o) shifterL shifterR n
  | n <= 64 = let bits_in_msb = 8 - o
                  (n',top) = (n - bits_in_msb
                             , (fromIntegral (unsafeHead bs) .&. make_mask bits_in_msb) `shifterL` (n - bits_in_msb))
                    
                  segs = byte_offset n

                  bn 1 = fromIntegral (unsafeIndex bs 1)
                  bn n = (bn (n-1) `shifterL` 8) .|. fromIntegral (unsafeIndex bs n)

                  mseg = bn segs

                  last | bit_offset n' > 0 = (fromIntegral (unsafeIndex bs (segs + 1))) `shifterR` (8 - (bit_offset n'))
                       | otherwise = 0

                  w = top .|. mseg .|. last
              in w :*: incS s n


readWord32be :: S -> Int -> T Word32 S
readWord32be s@(S bs o) n
  -- 8 or fewer bits, use readWord8
  | n <= 8 = let w :*: s' = readWord8 s n
             in fromIntegral w :*: s'

  -- 16 or fewer bits, use readWord16be
  | n <= 16 = let w :*: s' = readWord16be s n
              in fromIntegral w :*: s'

  | o == 0 = readWithoutOffset s shiftl_w32 shiftr_w32 n
            -- error "readWord32be: can't read without offset..."

  | n <= 32 = readWithOffset s shiftl_w32 shiftr_w32 n

  | otherwise = error "readWord32be: tried to read more than 32 bits"

{-

------------------------------------------------------------------------
-- unrolled codensity/state monad
newtype BitGet a = C { runCont :: forall r.
                                  S -> 
                                  Failure   r ->
                                  Success a r ->
                                  Either String r }

type Failure   r = String -> Either String r
type Success a r = S -> a -> Either String r

instance Monad BitGet where
  return x = C $ \s kf ks -> ks s x
  fail str = C $ \s kf ks -> kf str
  (C c) >>= f = C $ \s kf ks -> c s kf (\s' a -> runCont (f a) s' kf ks)

runBitGet :: BitGet a -> Int -> Get (Either String a)
runBitGet bg size = do
  bs <- getByteString size
  return $ runCont bg (S bs 0) (\str -> Left str) (\s a -> Right a)

getWord8 :: Int -> BitGet Word8
getWord8 = modifyState . flip readWord8
  
getWord16be :: Int -> BitGet Word16
getWord16be = modifyState . flip readWord16be

getWord32be :: Int -> BitGet Word32
getWord32be = modifyState . flip readWord32be
 
getState :: BitGet S
getState = C $ \s kf ks -> ks s s

putState :: S -> BitGet ()
putState s = C $ \_ kf ks -> ks s ()

modifyState :: (S -> (T a S)) -> BitGet a
modifyState f = C $ \s kf ks -> case f s of
                                  w :*: s' -> ks s' w
-}

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
