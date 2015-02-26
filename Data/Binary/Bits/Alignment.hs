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

module Data.Binary.Bits.Alignment
   ( Alignable(..)
   )
where

class Monad m => Alignable m where
   -- | Skip the given number of bits
   skipBits :: Int -> m ()

   -- | Skip bits if necessary to align to the next byte
   alignByte :: m ()
