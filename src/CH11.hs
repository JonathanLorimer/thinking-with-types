{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}


module CH11 where

import Data.Kind (Type)
import Data.Proxy (Proxy (..))
import qualified Data.Vector as V
import Fcf
import GHC.TypeLits hiding (type (+))
import GHC.OverloadedLabels (IsLabel (..))
import Unsafe.Coerce

data OpenSum (f :: k -> Type) (ts :: [k]) where
  UnsafeOpenSum :: Int -> f t -> OpenSum f ts

type FindElem (key :: k) (ts :: [k])
   = FromMaybe Stuck =<< FindIndex (TyEq key) ts

type Member t ts = KnownNat (Eval (FindElem t ts))

findElem ::
     forall t ts. Member t ts
  => Int
findElem = fromIntegral . natVal $ Proxy @(Eval (FindElem t ts))

inj :: forall f t ts . Member t ts => f t -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

prj :: forall f t ts . Member t ts => OpenSum f ts -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @t @ts
     then Just $ unsafeCoerce f
     else Nothing

decompose :: OpenSum f (t ': ts)
          -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right
                              $ UnsafeOpenSum (n - 1) t


weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

match
  :: forall f ts b
   . (forall t . f t -> b)
   -> OpenSum f ts
   -> b
match fn (UnsafeOpenSum _ t) = fn t

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f  :: k -> Type)
            (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) = Key

insert' :: Key key
       -> f t
       -> OpenProduct f ts
       -> OpenProduct f ('(key, t) ': ts)
insert' _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v


type UniqueKey (key :: k) (ts :: [(k, t)])
  = Null =<< Filter (TyEq key <=< Fst) ts


insert :: Eval (UniqueKey key ts) ~ 'True
       => Key key
       -> f t
       -> OpenProduct f ts
       -> OpenProduct f ('(key, t) ': ts)
insert _ ft (OpenProduct v) =
  OpenProduct $ V.cons (Any ft) v

