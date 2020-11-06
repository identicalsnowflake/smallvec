-- | SmallVec is just like Data.Vector, except it is backed by the SmallArray primitive, which performs slightly faster on the GHC runtime when arrays are small (<128 elements).

module Data.Vector.Small
       ( Vector
       ) where

import Data.Vector.Small.Internal

