{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE KindSignatures #-}

module CH12 where

import CH10
import CH11
import GHC.TypeLits

instance (TypeError (Text "Attempting to interpret a number as a function" :$$: Text "in the type `" :<>: ShowType (a -> b) :<>: Text "'" :$$: Text "Did you forget to specify the function you wanted?")) =>
         Num (a -> b)

type family FriendlyFindElem (f :: k -> Type) (t :: k) (ts :: [k]) where
  FriendlyFindElem f t ts = FromMaybe (TypeError ('Text "Attempted to call `friendlyPrj` to produce a `" ':<>: 'ShowType (f t) ':<>: 'Text "'." ':$$: 'Text "But the OpenSum can only contain one of:" ':$$: 'Text " " ':<>: 'ShowType ts)) =<< FindIndex (TyEq t) ts
