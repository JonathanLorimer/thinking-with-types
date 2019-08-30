{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}

module CH6 where

id :: forall a. a -> a
id a = a

applyToFive :: (forall a. a -> a) -> Int
applyToFive f = f 5

-- Exercise 6.3-i
-- Int -> forall a. a -> a
-- (Int -> (forall a. a)) -> a
-- Rank 1

-- Exercise 6.3-ii
-- (a -> b) -> ((forall c. c -> a) -> b)
-- Rank 2

-- Exercise 6.3-iii
-- ((forall x . m x -> b (z m x)) -> b (z m a)) -> m a
-- Rank 3


newtype Cont a =
  Cont { unCont :: forall r . ( a -> r ) -> r }


-- Exercise 6.4-i
instance Functor Cont where
  fmap ab (Cont arr) = Cont $ \br -> arr $ br . ab


-- abrr :: ((a -> b) -> r) -> r
-- arr :: (a -> r) -> r
-- Exercise 6.4-ii
-- instance Applicative Cont where
--   pure a                     = Cont $ \ar -> ar a
--   (Cont abrr) <*> (Cont arr) =
--     Cont $ \br -> arr $ abrr (\ab -> br . ab)


instance Applicative Cont where
  pure a                     = Cont $ \ar -> ar a
  (Cont abrr) <*> (Cont arr) =
    Cont $ \br -> abrr $ \ab -> arr $ br . ab

-- arr :: (a -> r) -> r
-- aCbrr :: a -> Cont ((b -> r) -> r)
-- Exercise 6.4.iii
instance Monad Cont where
  return = pure
  (>>=) (Cont arr) aCbrr =
    Cont $ \br -> arr $ \a -> (unCont . aCbrr) a br

-- Exercise 6.4-iv
newtype ContT m a =
  ContT { runContT :: forall r . (a -> m r) -> m r }

instance Functor m => Functor (ContT m) where
  fmap ab (ContT c) = ContT $ \br -> c $ br . ab

instance Applicative m => Applicative (ContT m) where
  pure a = ContT $ \amr -> amr a
  (ContT f) <*> (ContT a) =
    ContT $ \br -> f $ \ab -> a $ br . ab

instance Monad m => Monad (ContT m) where
  return = pure
  (ContT m) >>= f = ContT $ \br -> m $ \a -> (runContT . f) a br



