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

module CH7 where

import Data.IORef
import System.IO.Unsafe (unsafePerformIO)

-- | 7.1-i
-- No, because you cannot create an r from any `a`, so it just has to be chosen in the provided function

-- | 7.1-ii
-- You need the show constraint to be able to use show on s

-- | 7.1-iii
elimHasShow :: (forall a . Show a => a -> r)
            -> HasShow
            -> r
elimHasShow f (HasShow a) = f a

data HasShow where
  HasShow :: Show t => t -> HasShow

instance Show HasShow where
  show = elimHasShow show



