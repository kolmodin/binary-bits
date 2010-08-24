module BitsPut
          ( putBool
          , putWord8
          , runBitPut
          )
          where

import qualified Data.Binary.Builder as B
import Data.Binary.Builder ( Builder )
import qualified Data.Binary.Put as Put
import Data.ByteString 

import Data.Bits
import Data.Monoid
import Data.Word

data BitPut a = BitPut { run :: (S -> PairS a) }

data PairS a = PairS a {-# UNPACK #-} !S

data S = S !Builder !Word8 !Int

putBool :: Bool -> BitPut ()
putBool b = putWord8 1 (if b then 1 else 0)

putWord8 :: Int -> Word8 -> BitPut ()
putWord8 n w = BitPut $ \s -> PairS () $
  case s of
                -- a whole word8, no offset
    (S b t o) | n == 8 && o == 0 -> flush $ S b w n
                -- less than a word8, will fit in the current word8
              | n <= 8 - o       -> flush $ S b (t .|. (w `shiftL` (8 - n - o))) (o+n)
                -- will finish this word8, and spill into the next one
              | otherwise -> flush $
                              let o' = o + n - 8
                                  w' = t .|. (w `shiftR` o')
                                  t' = w `shiftL` (8 - o')
                              in S (b `mappend` B.singleton w') t' o'

putWord16be :: Int -> Word16 -> BitPut ()
putWord16be n w
  | n <= 8 = putWord8 n (fromIntegral w)
  | otherwise =
      BitPut $ \s -> PairS () $
        case s of
          (S b t o) | otherwise -> undefined

flush :: S -> S
flush s@(S b w o)
  | o > 8 = error "flush: offset > 8"
  | o == 8 = S (b `mappend` B.singleton w) 0 0
  | otherwise = s

runBitPut :: BitPut () -> Put.Put
runBitPut m = Put.putBuilder b'
  where
  PairS _ (S b t o) = run m (S mempty 0 0)
  b' | o == 0 = b
     | otherwise = b `mappend` B.singleton t

instance Monad BitPut where
  m >>= k = BitPut $ \s ->
    let PairS a s'  = run m s
        PairS b s'' = run (k a) s'
    in PairS b s''
  return x = BitPut $ \s -> PairS x s 
