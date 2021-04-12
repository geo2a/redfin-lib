{- |
 Module     : ISA.Types.ZeroOneTwo
 Copyright  : (c) Georgy Lukyanov 2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 A datatype holding either zero, one or two values of a type.
 Useful for encoding choices in symbolic execution
-}
module ISA.Types.ZeroOneTwo (ZeroOneTwo (..)) where

-- | Choice between no children, a single child node and a branch
data ZeroOneTwo a
    = Zero
    | One a
    | Two a a
    deriving (Functor, Eq)

instance Show a => Show (ZeroOneTwo a) where
    show = \case
        Zero -> "⊥"
        One a -> show a
        Two a b -> show (a, b)
