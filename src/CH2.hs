{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}

module CH2 where

import Data.Proxy
import GHC.TypeLits

-- | Exercises
-- | 2.1.3i
-- :k Show :: * -> Constraint
-- | 2.1.3ii
-- :k Functor :: (* -> *) -> Constraint
-- | 2.1.3ii
-- :k Monad :: (* -> *) -> Constraint
-- | 2.1.3iii
-- :k MonadTrans :: ((* -> *) -> * -> *) -> Constraint
-- 2.4i
type family Not (x :: Bool) :: Bool where
  Not 'True = 'False
  Not 'False = 'True

tf :: Proxy (Not 'True) -> Proxy 'False
tf = id
