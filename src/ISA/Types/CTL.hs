{-# LANGUAGE DeriveAnyClass #-}

module ISA.Types.CTL
 ( CTL(..)) where


import           Data.Aeson   (FromJSON, ToJSON)
import           GHC.Generics

-- | (Restricted) Computational Tree Logic formulae
--   https://functionallens.wordpress.com/2008/05/07/ctl-model-checking-in-haskell-a-classic-algorithm-explained-as-memoization/
data CTL p = TT | FF
           | Atom p
           | Not (CTL p)
           | And (CTL p) (CTL p)
           | AllG (CTL p)
        -- ^ "All Globally" holds in every consecutive state (of every consecutive path)
           | EG (CTL p)
        -- ^ "Exists Globally" holds if any there exists a state in the tree that
        --    satisfies p
           | AllF (CTL p)
        -- ^ "All Finally" eventually holds along every consecutive path
           deriving (Show, Generic, ToJSON, FromJSON)

-- -- | Forever on the future
-- allFuture f = AllUntil TT f

-- -- | Perhaps at some point
-- existsFuture f = ExUntil TT f

-- -- | All the time!
-- allGlobal f = Not(existsFuture(Not f))

-- -- | Sometime
-- existsGlobal f = Not(allFuture(Not f))
