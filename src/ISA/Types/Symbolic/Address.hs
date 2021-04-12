{- |
 Module     : ISA.Types.Symbolic.Address
 Copyright  : (c) Georgy Lukyanov 2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Manipulation of symbolic memory and program addresses
-}
module ISA.Types.Symbolic.Address (
    Address (..),
    literal,
    inc,
    Addressable (..),
) where

import Control.Applicative
import Data.Aeson
import Data.Int

import ISA.Types
import ISA.Types.Symbolic

newtype Address = MkAddress (Either CAddress Sym)
    deriving (Eq, Ord) via (Either CAddress Sym)

instance ToJSON Address where
    toJSON (MkAddress addr) = case addr of
        Left concrete -> toJSON concrete
        Right symbolic -> toJSON symbolic
instance FromJSON Address where
    parseJSON v =
        MkAddress . Left <$> (parseJSON @CAddress v)
            <|> MkAddress . Right <$> (parseJSON @Sym v)

-- -- We do not expect a non-Object value here.
-- -- We could use empty to fail, but typeMismatch
-- -- gives a much more informative error message.
-- parseJSON invalid    =
--     prependFailure "parsing Coord failed, "
--         (typeMismatch "Object" invalid)

instance Show Address where
    show = \case
        (MkAddress (Left (CAddress concrete))) -> show concrete
        (MkAddress (Right symbolic)) -> show symbolic

{- | The instance of @Num@ is defined only for
   initialising concrete addresses with numeric literals;
   everything else is an error
-}
instance Num Address where
    _ + _ = error "Address.Num: + is not defined"
    _ - _ = error "Address.Num: - is not defined"
    _ * _ = error "Address.Num: * is not defined"
    abs _ = error "Address.abs: abs is not defined"
    signum _ = error "Address.Num signum is not defined"
    fromInteger x = literal (fromInteger x)
    negate _ = error "Address.Num: negate is not defined"

-- | Embed a concrete address
literal :: CAddress -> Address
literal a = MkAddress (Left a)

{- | Increment an address concretely or symbolically, according to
   whether it is concrete or symbolic
-}
inc :: Address -> Address
inc (MkAddress x) = MkAddress $
    case x of
        Left concrete -> Left (concrete + 1)
        Right symbolic -> Right (SAdd symbolic 1)

{- | A typeclass representing things that could be converted to
   and from a memory location
-}
class Addressable a where
    toAddress :: a -> Maybe Address
    fromAddress :: Address -> a

instance Addressable Address where
    toAddress a = Just . id $ a
    fromAddress = id

instance Addressable CAddress where
    toAddress = Just . MkAddress . Left
    fromAddress (MkAddress x) =
        case x of
            Left a -> a
            Right sym ->
                error $
                    "Addressable.CAddress: can't interpret symbolic expression " <> show sym
                        <> " as a concrete memory address"

instance Addressable Concrete where
    toAddress c = case c of
        (CWord w) ->
            if w <= fromIntegral (maxBound :: CAddress)
                then Just (MkAddress (Left (CAddress (fromIntegral w))))
                else Nothing
        (CInt32 i) ->
            if i >= 0 && i <= fromIntegral (maxBound :: CAddress)
                then Just (MkAddress (Left (CAddress (fromIntegral i))))
                else Nothing
        (CBool _) -> Nothing
    fromAddress (MkAddress a) =
        case a of
            Left (CAddress concrete) -> CWord . fromIntegral $ concrete
            Right sym ->
                error $
                    "Addressable.(Data Concrete): can't interpret symbolic expression " <> show sym
                        <> " as a concrete word"

instance Addressable Sym where
    toAddress = Just . MkAddress . Right
    fromAddress (MkAddress a) =
        case a of
            Left (CAddress concrete) -> SConst (CWord $ fromIntegral concrete)
            Right sym -> sym

instance Addressable Int32 where
    toAddress x = toAddress (CInt32 x)
    fromAddress (MkAddress a) =
        case a of
            Left (CAddress concrete) -> fromIntegral concrete
            Right sym ->
                error $
                    "Addressable.(Data Int32): can't interpret symbolic expression " <> show sym
                        <> " as a concrete word"
