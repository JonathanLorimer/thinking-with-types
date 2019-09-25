{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeOperators #-}

module CH13 where

import GHC.Generics

class GEq a where
  geq :: a x -> a x -> Bool

instance GEq U1 where
  geq U1 U1 = True

instance GEq V1 where
  geq _ _ = True

instance Eq a => GEq (K1 _1 a) where
  geq (K1 a) (K1 b) = a == b

instance (GEq a, GEq b) => GEq (a :+: b) where
  geq (L1 a) (L1 b) = geq a b
  geq (R1 a) (R1 b) = geq a b
  geq _ _ = False

instance (GEq a, GEq b) => GEq (a :*: b) where
  geq (a1 :*: a2) (b1 :*: b2) = geq a1 b1 && geq a2 b2

instance GEq a => GEq (M1 _x _y a) where
  geq (M1 a1) (M1 a2) = geq a1 a2

genericEq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
genericEq a b = geq (from a) (from b)

-- | Exercise 13.2-i
class GOrd a where
  gord :: a x -> a x -> Ordering

instance GOrd U1 where
  gord U1 U1 = EQ

instance GOrd V1 where
  gord _ _ = EQ

instance Ord a => GOrd (K1 _1 a) where
  gord (K1 a) (K1 b) = compare a b

instance (GOrd a, GOrd b) => GOrd (a :+: b) where
  gord (L1 a) (L1 b) = gord a b
  gord (R1 a) (R1 b) = gord a b
  gord (R1 _) (L1 _) = GT
  gord (L1 _) (R1 _) = LT

instance (GOrd a, GOrd b) => GOrd (a :*: b) where
  gord (a1 :*: a2) (b1 :*: b2) = compare (gord a1 b1) (gord a2 b2)

instance GOrd a => GOrd (M1 _x _y a) where
  gord (M1 a1) (M1 a2) = gord a1 a2

genericOrd :: (Generic a, GOrd (Rep a)) => a -> a -> Ordering
genericOrd a b = gord (from a) (from b)

-- | Exercise 13.2-ii
class GExNihilo a where
  gexnihilo :: Maybe (a x)

instance GExNihilo U1 where
  gexnihilo = Just U1

instance GExNihilo V1 where
  gexnihilo = Nothing

instance GExNihilo (K1 _1 a) where
  gexnihilo = Nothing

instance GExNihilo (a :+: b) where
  gexnihilo = Nothing

instance GExNihilo (a :*: b) where
  gexnihilo = Nothing

instance GExNihilo a => GExNihilo (M1 _x _y a) where
  gexnihilo = fmap M1 gexnihilo
