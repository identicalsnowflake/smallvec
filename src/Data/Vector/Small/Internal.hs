module Data.Vector.Small.Internal (MVector(..) , Vector(..)) where

import Control.Applicative
import Control.DeepSeq
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Control.Monad.Zip
import qualified Data.Primitive.SmallArray as SA
import Data.Foldable
import Data.Functor.Classes (Eq1 (..) , Ord1 (..) , Read1 (..) , Show1 (..))
import Data.Semigroup
import qualified Data.Vector.Fusion.Bundle as Bundle
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Generic.Mutable as MGV
import GHC.Exts hiding (toList)
import qualified GHC.Exts as Exts (IsList(..))
import Text.Read (Read(..) , readListPrecDefault)


data MVector s a = MVector {
    mbuffer :: !(SmallMutableArray# s a)
  , moffset :: {-# UNPACK #-} !Int
  , mcount :: {-# UNPACK #-} !Int
  }

instance MGV.MVector MVector a where
  {-# INLINE basicLength #-}
  basicLength = mcount

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice s l (MVector mb mo _) = MVector mb (mo + s) l

  {-# INLINE basicOverlaps #-}
  basicOverlaps (MVector mba1 mo1 mc1) (MVector mba2 mo2 mc2) = case sameSmallMutableArray# mba1 mba2 of
    0# -> False
    _ -> do
      let e1 = mo1 + mc1
          e2 = mo2 + mc2
      bet mo1 mo2 e2 || bet e1 mo2 e2
    where
      bet x a b = x >= a && x <= b

  {-# INLINE basicUnsafeNew #-}
  basicUnsafeNew i = primitive \s -> do
    let !(I# c) = i
        !(# s1 , mba #) = newSmallArray# c undefined s
    (# s1 , MVector mba 0 i #)

  {-# INLINE basicInitialize #-}
  basicInitialize _ = pure ()

  {-# INLINE basicUnsafeRead #-}
  basicUnsafeRead (MVector mb mo _) i = do
    SA.readSmallArray (SA.SmallMutableArray mb) (mo + i)

  {-# INLINE basicUnsafeWrite #-}
  basicUnsafeWrite (MVector mb mo _) i v =
    SA.writeSmallArray (SA.SmallMutableArray mb) (mo + i) v

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector tmb o1 _) (MVector smb so c) =
    SA.copySmallMutableArray (SA.SmallMutableArray tmb) o1 (SA.SmallMutableArray smb) so c

  {-# INLINE basicUnsafeMove #-}
  basicUnsafeMove (MVector tmb o1 _) (MVector smb so c) =
    SA.copySmallMutableArray (SA.SmallMutableArray tmb) o1 (SA.SmallMutableArray smb) so c

type instance GV.Mutable Vector = MVector


data Vector a = Vector {
    buffer :: !(SmallArray# a)
  , offset :: {-# UNPACK #-} !Int
  , count :: {-# UNPACK #-} !Int
  }

{-# INLINE liftRnfV #-}
liftRnfV :: (a -> ()) -> Vector a -> ()
liftRnfV elemRnf = foldl' (\_ -> elemRnf) ()

instance Exts.IsList (Vector a) where
  type Item (Vector a) = a
  {-# INLINE fromList #-}
  fromList = GV.fromList
  {-# INLINE fromListN #-}
  fromListN = GV.fromListN
  {-# INLINE toList #-}
  toList = toList

instance NFData a => NFData (Vector a) where
  rnf = liftRnfV rnf
  {-# INLINEABLE rnf #-}

instance NFData1 Vector where
  liftRnf = liftRnfV
  {-# INLINEABLE liftRnf #-}

instance Semigroup (Vector a) where
  {-# INLINE (<>) #-}
  (<>) = (GV.++)
  {-# INLINE sconcat #-}
  sconcat = GV.concatNE

instance Monoid (Vector a) where
  {-# INLINE mempty #-}
  mempty = GV.empty

  {-# INLINE mappend #-}
  mappend = (GV.++)

  {-# INLINE mconcat #-}
  mconcat = GV.concat

instance Eq a => Eq (Vector a) where
  {-# INLINE (==) #-}
  xs == ys = Bundle.eq (GV.stream xs) (GV.stream ys)
  {-# INLINE (/=) #-}
  xs /= ys = not (Bundle.eq (GV.stream xs) (GV.stream ys))

instance Ord a => Ord (Vector a) where
  {-# INLINE compare #-}
  compare xs ys = Bundle.cmp (GV.stream xs) (GV.stream ys)
  {-# INLINE (<) #-}
  xs < ys = Bundle.cmp (GV.stream xs) (GV.stream ys) == LT
  {-# INLINE (<=) #-}
  xs <= ys = Bundle.cmp (GV.stream xs) (GV.stream ys) /= GT
  {-# INLINE (>) #-}
  xs > ys = Bundle.cmp (GV.stream xs) (GV.stream ys) == GT
  {-# INLINE (>=) #-}
  xs >= ys = Bundle.cmp (GV.stream xs) (GV.stream ys) /= LT

instance Show a => Show (Vector a) where
  {-# INLINE showsPrec #-}
  showsPrec = GV.showsPrec

instance Read a => Read (Vector a) where
  {-# INLINE readPrec #-}
  readPrec = GV.readPrec
  {-# INLINE readListPrec #-}
  readListPrec = readListPrecDefault

instance Eq1 Vector where
  {-# INLINE liftEq #-}
  liftEq eq xs ys = Bundle.eqBy eq (GV.stream xs) (GV.stream ys)

instance Ord1 Vector where
  {-# INLINE liftCompare #-}
  liftCompare cmp xs ys = Bundle.cmpBy cmp (GV.stream xs) (GV.stream ys)

instance Read1 Vector where
  {-# INLINE liftReadsPrec #-}
  liftReadsPrec = GV.liftReadsPrec

instance Show1 Vector where
  {-# INLINE liftShowsPrec #-}
  liftShowsPrec = GV.liftShowsPrec

instance MonadZip Vector where
  {-# INLINE mzip #-}
  mzip = GV.zip

  {-# INLINE mzipWith #-}
  mzipWith = GV.zipWith

  {-# INLINE munzip #-}
  munzip = GV.unzip

instance Alternative Vector where
  {-# INLINE empty #-}
  empty = GV.empty

  {-# INLINE (<|>) #-}
  (<|>) = (GV.++)

instance MonadPlus Vector where
  {-# INLINE mzero #-}
  mzero = GV.empty

  {-# INLINE mplus #-}
  mplus = (GV.++)

instance Applicative Vector where
  {-# INLINE pure #-}
  pure x = runST do
    mv <- MGV.unsafeNew 1
    MGV.unsafeWrite mv 0 x
    GV.unsafeFreeze mv

  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Monad Vector where
  {-# INLINE return #-}
  return = pure
  {-# INLINE (>>=) #-}
  (>>=) = flip GV.concatMap

instance MonadFail Vector where
  {-# INLINE fail #-}
  fail _ = GV.empty

instance Functor Vector where
  {-# INLINE fmap #-}
  fmap f v = runST do
    let l = GV.length v
    mv <- MGV.unsafeNew l
    for 0 (<) l \i -> do
      x <- GV.unsafeIndexM v i
      MGV.unsafeWrite mv i (f x)
    GV.unsafeFreeze mv

instance Foldable Vector where
  {-# INLINE foldr #-}
  foldr = GV.foldr

  {-# INLINE foldl #-}
  foldl = GV.foldl

  {-# INLINE foldr1 #-}
  foldr1 = GV.foldr1

  {-# INLINE foldl1 #-}
  foldl1 = GV.foldl1
  
  {-# INLINE foldr' #-}
  foldr' = GV.foldr'

  {-# INLINE foldl' #-}
  foldl' = GV.foldl'

  {-# INLINE toList #-}
  toList = GV.toList

  {-# INLINE length #-}
  length = GV.length

  {-# INLINE null #-}
  null = GV.null

  {-# INLINE elem #-}
  elem = GV.elem

  {-# INLINE maximum #-}
  maximum = GV.maximum

  {-# INLINE minimum #-}
  minimum = GV.minimum

  {-# INLINE sum #-}
  sum = GV.sum

  {-# INLINE product #-}
  product = GV.product
  
instance Traversable Vector where
  {-# INLINE traverse #-}
  traverse f v = do
    let !n = GV.length v
    GV.fromListN n <$> traverse f (GV.toList v)

{-# INLINE for #-}
for :: Applicative f => Int -> (Int -> Int -> Bool) -> Int -> (Int -> f ()) -> f ()
for s b e f = loop s
  where
    loop i = if b i e
      then f i *> loop (i + 1)
      else pure ()


instance GV.Vector Vector a where
  {-# INLINE basicUnsafeFreeze #-}
  basicUnsafeFreeze (MVector mba o c) = do
    SA.SmallArray ba <- SA.unsafeFreezeSmallArray (SA.SmallMutableArray mba)
    pure (Vector ba o c)

  {-# INLINE basicUnsafeThaw #-}
  basicUnsafeThaw (Vector ba o c) = do
    SA.SmallMutableArray mba <- SA.unsafeThawSmallArray (SA.SmallArray ba)
    pure (MVector mba o c)

  {-# INLINE basicLength #-}
  basicLength (Vector _ _ c) = c

  {-# INLINE basicUnsafeSlice #-}
  basicUnsafeSlice s l (Vector ba o _) = Vector ba (o + s) l

  {-# INLINE basicUnsafeIndexM #-}
  basicUnsafeIndexM (Vector ba o _) i =
    SA.indexSmallArrayM (SA.SmallArray ba) (o + i)

  {-# INLINE basicUnsafeCopy #-}
  basicUnsafeCopy (MVector mba o1 _) (Vector ba o2 c) =
    SA.copySmallArray (SA.SmallMutableArray mba) o1 (SA.SmallArray ba) o2 c

