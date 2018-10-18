{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | Export a few orphan instances
module Linear.Storable
  (
  ) where

import Foreign.Storable.Generic
import Linear

instance GStorable a => GStorable (V2 a)
instance GStorable a => GStorable (V3 a)
instance GStorable a => GStorable (V4 a)
