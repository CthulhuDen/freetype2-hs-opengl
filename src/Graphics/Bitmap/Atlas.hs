{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}

-- | Description: Capabilities to combine multiple bitmaps into an atlas
-- Module provides bitmaps atlas able to combine multiple input bitmaps,
-- assigning page number/coordinates for each new input bitmap.
module Graphics.Bitmap.Atlas
  ( -- * Data Types
    Size (..)
  , AtlasConfig (..)
  , BitmapAtlas
    -- * Allocating and updating atlas
  , newAtlas
  , insertBitmap
    -- * Querying atlas
  , countPages
  , withPages
  ) where

import Control.Monad
import qualified Data.Bitmap.IO as B
import Data.List (foldl')
import Data.Maybe
import Data.Word
import Foreign.ForeignPtr
import Foreign.Ptr
import Prelude hiding (pi)

-- | Size of a bitmap
data Size = Size !Int !Int

-- | Lane in the atlas - height and current allocated width
data Lane = Lane !Int !Int
  deriving Show

type Page = [Lane]

data Atlas = Atlas ![Page] !AtlasConfig

-- | Configuration for an atlas
data AtlasConfig = AtlasConfig
  { atlasPageWidth :: !Int
  , atlasPageHeight :: !Int
  , atlasOverReserve :: !Double
  , atlasMaxOverReserve :: !Double
  }

insertBlock :: Size -> Atlas -> (Int, (Int, Int), Atlas)
insertBlock (Size bw bh) (Atlas ps ac@AtlasConfig {..}) =
  let
    -- Removed those from where section to avoid names shadowing
    (y, ps') = case splitAt pi ps of
      (pB, [])    -> (0, pB <> [[l]])
      (pB, ls:pA) -> case splitAt li ls of
        (lB, [])   -> (sumLanesH lB, pB <> ((lB <> [l]) : pA))
        (lB, _:lA) -> (sumLanesH lB, pB <> ((lB <> (l:lA)) : pA))
    (l, (pi, li, x)) = fromMaybe addLane findLane
  in (pi, (x, y), Atlas ps' ac)
  where
    addLane = go ps (0 :: Int)
      where
        go [] i                = (Lane (min atlasPageHeight desiredHeight) bw, (i, 0, 0))
        go (ls:ps') !i
          | heightReserve < bh = go ps' (i + 1)
          | otherwise          = (Lane (min heightReserve desiredHeight) bw, (i, length ls, 0))
          where
            heightReserve = atlasPageHeight - sumLanesH ls
    desiredHeight = ceiling $ fromIntegral bh * (1 + atlasOverReserve)
    sumLanesH ls = sum [l'h | Lane l'h _ <- ls]
    findLane = chooseBest $ catMaybes $ zipWith addPageIx (findLaneInPage <$> ps) [0 :: Int ..]
      where
        addPageIx Nothing _              = Nothing
        addPageIx (Just (l, (li, x))) pi = Just (l, (pi, li, x))
    findLaneInPage ls = chooseBest $ catMaybes $ zipWith addLaneIx (tryLane <$> ls) [0 :: Int ..]
      where
        addLaneIx Nothing _        = Nothing
        addLaneIx (Just (l, x)) li = Just (l, (li, x))
    chooseBest = foldl' go Nothing
      where
        go Nothing la = Just la
        go (Just la1@(Lane l1h _, _)) la2@(Lane l2h _, _)
          | l2h < l1h = Just la2
          | otherwise = Just la1
    tryLane (Lane lh lw)
      | lh < bh                                                    = Nothing
      | lh > ceiling (fromIntegral bh * (1 + atlasMaxOverReserve)) = Nothing
      | lw + bw > atlasPageWidth                                   = Nothing
      | otherwise                                                  = Just (Lane lh (lw + bw), lw)

-- | Atlas stores pages which contain combined bitmap.
-- For each new bitmap user receives page number and in-page coordinates.
--
-- The pages can then be consumed using 'withPages'
data BitmapAtlas = BitmapAtlas Atlas [B.IOBitmap Word8]

-- | Construct new bitmap atlas
newAtlas :: AtlasConfig -> BitmapAtlas
newAtlas ac = BitmapAtlas (Atlas [] ac) []

-- | Combine a new bitmap into an atlas
insertBitmap :: Size -> Ptr Word8 -> BitmapAtlas -> IO (Int, (Int, Int), BitmapAtlas)
insertBitmap s@(Size w h) ptr (BitmapAtlas a@(Atlas _ AtlasConfig {..}) bs) = do
  (bB, b, bA) <- case splitAt pi bs of
    (bB, [])   -> do
      b <- B.emptyBitmap (atlasPageWidth, atlasPageHeight) 1 Nothing
      pure (bB, b, [])
    (bB, b:bA) -> pure (bB, b, bA)
  b' <- B.ioBitmapFromForeignPtrUnsafe (w, h) 1 1 0 <$> newForeignPtr_ ptr
  B.copySubImageInto b' (0, 0) (w, h) b (x, y)
  pure (pi, (x, y), BitmapAtlas a' $ bB <> (b:bA))
  where
    (pi, (x, y), a') = insertBlock s a

-- | Get count of pages allocated by given atlas.
countPages :: Integral int => BitmapAtlas -> int
countPages (BitmapAtlas _ ps) = fromIntegral $ length ps

-- | Iterate all the pages of the given atlas.
withPages :: BitmapAtlas -> (Int -> Size -> Ptr Word8 -> IO a) -> IO [a]
withPages (BitmapAtlas _ bs) act = forM (zip [0..] bs) $ \(i, b) ->
  B.withIOBitmap b $ \(w, h) _align _pad ptr -> act i (Size w h) ptr
