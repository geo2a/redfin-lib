{- |
 Module     : ISA.Types.SBV.SFunArray
 Copyright  : (c) Levent Erkok, Andrey Mokhov, Georgy Lukyanov 2017-2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Backport of the old implementation of SFunArray from SBV <= 7.8. The original
 implementation is due to Levent Erkok
-}
module ISA.Types.SBV.SFunArray (
    SFunArray (..),
    mkSFunArray,
    readArray,
    writeArray,
    sListArray,
) where

import Data.Foldable (foldl')
import Data.SBV hiding (SArray, SFunArray, SymArray (..))
import qualified Data.SBV as SBV

newtype SFunArray a b = SFunArray (SBV a -> SBV b)

instance (HasKind a, HasKind b) => Show (SFunArray a b) where
    show (SFunArray _) =
        "SFunArray<" ++ showType (undefined :: a)
            ++ ":"
            ++ showType (undefined :: b)
            ++ ">"

mkSFunArray :: (SBV a -> SBV b) -> SFunArray a b
mkSFunArray = SFunArray

readArray :: SFunArray a b -> SBV a -> SBV b
readArray (SFunArray f) = f

writeArray :: SymVal b => SFunArray a b -> SBV a -> SBV b -> SFunArray a b
writeArray (SFunArray f) a b = SFunArray (\a' -> ite (a .== a') b (f a'))

mergeArrays :: SymVal b => SBV Bool -> SFunArray a b -> SFunArray a b -> SFunArray a b
mergeArrays t (SFunArray g) (SFunArray h) = SFunArray (\x -> ite t (g x) (h x))

sListArray :: forall a b. (HasKind a, SymVal b) => b -> [(SBV a, SBV b)] -> SFunArray a b
sListArray init = foldl' (uncurry . writeArray) emptyArr
  where
    emptyArr = mkSFunArray (const . literal $ init)

instance SymVal b => Mergeable (SFunArray a b) where
    symbolicMerge _ = mergeArrays

{- | The reason why I need the old SFunArray is that for both SArray and the new
   SFunArray, the values read from the array, even if literal, become explicitly
   symbolic; hence show up as <symbolic> when printed. This makes impossible my
   old workflow of simulation by symbolic execution with concrete values.
-}
sArrayEx :: Symbolic SWord8
sArrayEx = do
    a <- SBV.newArray_ @SBV.SArray @Word8 Nothing
    let a' = SBV.writeArray a 0 1
    pure $ SBV.readArray a' 0

-- | The new SFunArray suffers from the same issue that SArray
sFunArraySBVEx :: Symbolic SWord8
sFunArraySBVEx = do
    a <- SBV.newArray_ @SBV.SFunArray @Word8 Nothing
    let a' = SBV.writeArray a 0 1
    pure $ SBV.readArray a' 0

-- | The old SFunArray keeps literal values concrete
sFunArrayEx :: Symbolic SWord8
sFunArrayEx = do
    let a = mkSFunArray @Word8 @Word8 (const . literal $ 0)
    let a' = writeArray a 0 1
    pure $ readArray a' 0
