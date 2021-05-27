{- |
 Module     : ISA.Semantics.Overflow
 Copyright  : (c) Georgy Lukyanov 2021
 License    : MIT (see the file LICENSE)
 Maintainer : mail@gmail.com
 Stability  : experimental

 Checks for integer overflow in arithmetic operations
-}
module ISA.Semantics.Overflow (
    -- * integer addition overflows
    addOverflows,

    -- * integer subtraction overflows
    subOverflows,

    -- * integer multiplication overflows
    mulOverflows,

    -- * div overflows
    divOverflows,

    -- * mod overflows
    modOverflows,

    -- * abs overflows
    absOverflows,
) where

import ISA.Types.Boolean

addOverflows :: (Num a, Bounded a, Boolean a, BOrd a) => a -> a -> a
addOverflows x y =
    let o1 = gt y 0
        o2 = gt x (maxBound - y)
        o3 = lt y 0
        o4 = lt x (minBound - y)
     in (|||)
            ((&&&) o1 o2)
            ((&&&) o3 o4)

subOverflows :: (Num a, Bounded a, Boolean a, BOrd a) => a -> a -> a
subOverflows arg1 arg2 =
    let o1 = arg2 `gt` 0
        o2 = arg1 `lt` (minBound + arg2)
        o3 = arg2 `lt` 0
        o4 = arg1 `gt` (maxBound + arg2)
     in (|||)
            ((&&&) o1 o2)
            ((&&&) o3 o4)

mulOverflows :: (Integral a, Bounded a, Boolean a, BOrd a) => a -> a -> a
mulOverflows arg1 arg2 =
    let term1_1 = arg1 `gt` 0
        term1_2 = arg2 `gt` 0
        term1_3 = arg1 `gt` (div maxBound arg2)
        term2_1 = arg1 `gt` 0
        term2_2 = (arg2 `lt` 0) ||| (arg2 === 0)
        term2_3 = arg2 `lt` (div minBound arg1)
        term3_1 = (arg1 `lt` 0) ||| (arg1 === 0)
        term3_2 = arg2 `gt` 0
        term3_3 = arg1 `lt` (div minBound arg2)
        term4_1 = arg1 `lt` 0
        term4_2 = (arg2 `lt` 0) ||| (arg2 === 0)
        term4_3 = arg2 `lt` (div maxBound arg1)
     in (term1_1 &&& term1_2 &&& term1_3)
            ||| (term2_1 &&& term2_2 &&& term2_3)
            ||| (term3_1 &&& term3_2 &&& term3_3)
            ||| (term4_1 &&& term4_2 &&& term4_3)

divOverflows :: (Integral a, Bounded a, Boolean a, BOrd a) => a -> a -> a
divOverflows arg1 arg2 =
    (arg1 === minBound) &&& (arg2 === (0 - 1))

modOverflows :: (Integral a, Bounded a, Boolean a, BOrd a) => a -> a -> a
modOverflows arg1 arg2 =
    (arg1 === minBound) &&& (arg2 === (0 - 1))

absOverflows :: (Bounded a, Boolean a, BEq a) => a -> a
absOverflows arg = arg === minBound
