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
import Data.Proxy (Proxy(..))
import qualified Data.Vector as V
import Fcf
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits hiding (type (+))
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

inj ::
     forall f t ts. Member t ts
  => f t
  -> OpenSum f ts
inj = UnsafeOpenSum (findElem @t @ts)

prj ::
     forall f t ts. Member t ts
  => OpenSum f ts
  -> Maybe (f t)
prj (UnsafeOpenSum i f) =
  if i == findElem @t @ts
    then Just $ unsafeCoerce f
    else Nothing

decompose :: OpenSum f (t ': ts) -> Either (f t) (OpenSum f ts)
decompose (UnsafeOpenSum 0 t) = Left $ unsafeCoerce t
decompose (UnsafeOpenSum n t) = Right $ UnsafeOpenSum (n - 1) t

weaken :: OpenSum f ts -> OpenSum f (x ': ts)
weaken (UnsafeOpenSum n t) = UnsafeOpenSum (n + 1) t

match :: forall f ts b. (forall t. f t -> b) -> OpenSum f ts -> b
match fn (UnsafeOpenSum _ t) = fn t

data Any (f :: k -> Type) where
  Any :: f t -> Any f

data OpenProduct (f :: k -> Type) (ts :: [(Symbol, k)]) where
  OpenProduct :: V.Vector (Any f) -> OpenProduct f ts

nil :: OpenProduct f '[]
nil = OpenProduct V.empty

data Key (key :: Symbol) =
  Key

insert' ::
     Key key -> f t -> OpenProduct f ts -> OpenProduct f ('( key, t) ': ts)
insert' _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type UniqueKey (key :: k) (ts :: [(k, t)])
   = Null =<< Filter (TyEq key <=< Fst) ts

insert ::
     Eval (UniqueKey key ts) ~ 'True
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f ('( key, t) ': ts)
insert _ ft (OpenProduct v) = OpenProduct $ V.cons (Any ft) v

type FindElem' (key :: Symbol) (ts :: [(Symbol, k)])
   = Eval (FromMaybe Stuck =<< FindIndex (TyEq key <=< Fst) ts)

findElem' ::
     forall key ts. KnownNat (FindElem' key ts)
  => Int
findElem' = fromIntegral . natVal $ Proxy @(FindElem' key ts)

type LookupType (key :: k) (ts :: [(k, t)]) = FromMaybe Stuck =<< Lookup key ts

get ::
     forall key ts f. KnownNat (FindElem' key ts)
  => Key key
  -> OpenProduct f ts
  -> f (Eval (LookupType key ts))
get _ (OpenProduct v) = unAny $ V.unsafeIndex v $ findElem' @key @ts
  where
    unAny (Any a) = unsafeCoerce a

type UpdateElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)])
   = SetIndex (FindElem' key ts) '( key, t) ts

update ::
     forall key ts t f. KnownNat (FindElem' key ts)
  => Key key
  -> f t
  -> OpenProduct f ts
  -> OpenProduct f (Eval (UpdateElem key t ts))
update _ ft (OpenProduct v) =
  OpenProduct $ v V.// [(findElem' @key @ts, Any ft)]

-- | Exercise 11.3i
type DeleteElem key = Filter (Not <=< TyEq key <=< Fst)

delete ::
     forall key ts t f. KnownNat (FindElem' key ts)
  => Key key
  -> OpenProduct f ts
  -> OpenProduct f (Eval (DeleteElem key ts))
delete _ (OpenProduct v) =
  let (a, b) = V.splitAt (findElem' @key @ts) v
   in OpenProduct $ a V.++ V.tail b

-- | Exercise 11.3-ii
data Placeholder1Of3 :: (a -> b -> c -> Exp r) -> b -> c -> a -> Exp r where

type instance Eval (Placeholder1Of3 f b c a) = Eval (f b c a)

type UpsertElem (key :: Symbol) (t :: k) (ts :: [(Symbol, k)]) =
     FromMaybe ('( key, t) ': ts)
       =<< Map (Placeholder1Of3 SetIndex '(key, t) ts)
       =<< FindIndex (TyEq key <=< Fst) ts

type UpsertLoc (key :: Symbol) (ts :: [(Symbol, k)])
   = Eval (FindIndex (TyEq key <=< Fst) ts)

class FindUpsertElem (a :: Maybe Nat) where
  upsertElem :: Maybe Int

instance FindUpsertElem 'Nothing where
  upsertElem = Nothing

instance KnownNat n => FindUpsertElem ('Just n) where
  upsertElem = Just . fromIntegral . natVal $ Proxy @n

upsert :: forall key ts t f
        . FindUpsertElem (UpsertLoc key ts)
       => Key key
       -> f t
       -> OpenProduct f ts
       -> OpenProduct f (Eval (UpsertElem key t ts))
upsert k ft (OpenProduct v) =
  OpenProduct $ case upsertElem @(UpsertLoc key ts) of
                  Nothing -> V.cons (Any ft) v
                  Just n -> v V.// [(n, Any ft)]
