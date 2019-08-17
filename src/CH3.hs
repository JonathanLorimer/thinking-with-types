module CH3 where

-- | Exercise 3i
newtype T1 a =
  T1 (Int -> a)

instance Functor T1 where
  fmap f (T1 g) = T1 $ f . g

newtype T2 a =
  T2 (a -> Int)

-- This is a contravariant functor
newtype T3 a =
  T3 (a -> a)

-- This is an invariant functor
newtype T4 a =
  T4 ((Int -> a) -> Int)

-- Cant make a functor
newtype T5 a =
  T5 ((a -> Int) -> Int)

instance Functor T5 where
  fmap f (T5 aii) = T5 $ \bi -> aii $ bi . f
