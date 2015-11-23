{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Vision.Image.Transform.ScaleDCT
-- Copyright   :  (C) 2015 Oleg Grenrus, 2015 Alex Mason
-- License     :  BSD3
-- Maintainer  :  Alex Mason <axman6@gmail.com>
--
-- Scale pictures using Discrete Cosine Transform.
--
module Vision.Image.Transform.ScaleDCT (scale) where

import           Prelude                ()
import           Prelude.Compat

import           Data.Array.CArray.Base (CArray (..))
import qualified Data.Vector.Storable   as VS
import           Vision.Image           hiding ((!))
                                        -- (Delayed (..), FromFunction (..), Image,
                                        --  ImagePixel, Manifest (..),
                                        --  RGBAPixel (..), compute, index, shape)
import           Vision.Primitive.Shape

import           Data.Array.CArray      (amap, array, bounds, elems, size, (!))
import           Data.Ix                (range)
import           Math.FFT               (dct2N, dct3N)

import qualified Data.Vector            as V


type Array2D = CArray (Int, Int) Double


{-# INLINE scale #-}
{-# SPECIALIZE scale :: (Int,Int) -> Manifest RGBAPixel -> Manifest RGBAPixel #-}
{-# SPECIALIZE scale :: (Int,Int) -> Manifest RGBPixel -> Manifest RGBPixel #-}
scale :: ( ImagePixel i ~ pix
          , PixelChannel pix ~ pixChan
          , Integral pixChan
          , Pixel pix
          , VS.Storable pixChan
          , Image i)
      => (Int, Int)         -- ^ Output width, height
      -> i                  -- ^ Input image
      -> Manifest (ImagePixel i) -- ^ Output image
-- scale :: (Int,Int) -> Manifest RGBAPixel -> Manifest RGBAPixel
scale dim@(w,h) img = Manifest (Z :. w :. h) res'
  where

    Manifest ((Z :. iw) :. ih) ivec = compute img

    -- ivec' :: (VS.Storable pixChan) => VS.Vector pixChan
    !ivec' = castVec img ivec

    !nchans = nChannels img

    -- This is necessary to ensure that we don't constantly do a lookup
    -- for which implementation of fromIntegral we want to use in chanVec
    -- below
    fi :: (Integral pixChan) => pixChan -> Double
    fi x = fromIntegral x

    chanVec = V.generate nchans (\chan ->
                imageToArray (Manifest ((Z :. iw) :. ih) $
                    VS.generate (iw*ih) $ \ix ->
                        fi $ VS.unsafeIndex ivec' (ix * nchans + chan)))

    chanVec' :: V.Vector Array2D
    chanVec' = fmap transform chanVec

    {-# INLINE truncate' #-}
    truncate' :: (Image i, Integral (PixelChannel (ImagePixel i)), VS.Storable (ImagePixel i))
              => i
              -> Double
              -> PixelChannel (ImagePixel i)
    truncate' _ = truncate

    -- res :: (VS.Storable t', Integral t', t' ~ t) => VS.Vector t'
    res = VS.generate (nchans * h * w) (\ix -> case quotRem ix nchans of
                (i,v) -> truncate' img . limit $ V.unsafeIndex chanVec' v ! (quotRem i w)
                )

    res' :: (VS.Storable pix) => VS.Vector pix
    res' = VS.unsafeCast res

    {-# INLINE castVec #-}
    castVec :: (ImagePixel i ~ pix', Image i, VS.Storable pix', VS.Storable (PixelChannel pix'))
            => i
            -> VS.Vector pix'
            -> VS.Vector (PixelChannel pix')
    castVec _ = VS.unsafeCast

    transform ch = amap (k*) ch'
      where
        ch' = dct3N [1, 0] . cut dim . dct2N [0, 1] $ ch
        k   = imgNorm ch / imgNorm ch'


{-# INLINE imgNorm #-}
imgNorm :: Array2D -> Double
imgNorm ch = sqrt . (/n) . sum . fmap sq . elems $ ch
  where sq x = x * x
        n = fromIntegral $ size ch

{-# INLINE cut #-}
cut :: (Int, Int) -> Array2D -> Array2D
cut (w, h) img = array b [ (i, pick i) | i <- range b ]
  where b            = ((0,0), (h-1, w-1))

        (_,(w',h'))  = bounds img
        pick i@(x,y) | x < h' && y < w' = img ! i
                     | otherwise    = 0


{-# INLINE imageToArray #-}
imageToArray :: Manifest Double -> Array2D
imageToArray img = case img of
    Manifest ((Z :. h) :. w) vec -> case VS.unsafeToForeignPtr0 vec of
        (fptr, len) -> CArray (0,0) (h-1,w-1) len fptr

{-# INLINE limit #-}
limit :: Double -> Double
limit x | x < 0     = 0
        | x > 255   = 255
        | otherwise = x

-- -- From 'Control.Lens.Lens' from 'lens' package
-- toListOf :: Traversal s s a a -> s -> [a]
-- toListOf l = foldrOf l (:) []
-- {-# INLINE toListOf #-}

-- foldrOf :: Traversal s s a a -> (a -> r -> r) -> r -> s -> r
-- foldrOf l f z = flip appEndo z . foldMapOf l (Endo #. f)
-- {-# INLINE foldrOf #-}

-- foldMapOf :: Monoid r => Traversal s s a a -> (a -> r) -> s -> r
-- foldMapOf l f = getConst #. l (Const #. f)
-- {-# INLINE foldMapOf #-}

-- (#.) :: Coercible c b => (b -> c) -> (a -> b) -> a -> c
-- (#.) _ = coerce (\x -> x :: b) :: forall a b. Coercible b a => a -> b
-- {-# INLINE (#.) #-}

--(.#) :: Coercible b a => (b -> c) -> (a -> b) -> a -> c
--(.#) pbc _ = coerce pbc
