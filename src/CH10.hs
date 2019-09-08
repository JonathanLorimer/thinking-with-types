{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE TypeOperators #-}


module CH10 where

import Data.Kind (Constraint, Type)
import Prelude hiding (fst)

-- Exercise 10.1-i
data ListToMaybe a =
  ListToMaybe [a]

class Eval l t | l -> t where
  eval :: l -> t

instance Eval (ListToMaybe a) (Maybe a) where
  eval (ListToMaybe []) = Nothing
  eval (ListToMaybe (x:_)) = Just x

-- Exercise 10.2-i
type Exp a = a -> Type

type family Eval' (e :: Exp a) :: a

data ListToMaybe' :: [a] -> Exp (Maybe a)

type instance Eval' (ListToMaybe' '[]) = 'Nothing
type instance Eval' (ListToMaybe' '[a]) = 'Just a

-- Exercise 10.2-ii
data FoldR :: (a -> b -> Exp b) -> b -> [a] -> Exp b
type instance Eval' (FoldR _ z '[]) = z
type instance Eval' (FoldR f z (x ': xs)) =
  Eval' (f x (Eval' (FoldR f z xs)))

-- Exercise 10.4-ii
data Map :: (a -> Exp b) -> f a -> Exp (f b)
type instance Eval' (Map f '(a, b)) = '(a, Eval' (f b))

