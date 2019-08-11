module CH1 where

-- | 1.2
{-
 Either Bool (Bool, Maybe Bool) -> Bool
 Bool | (Bool, Bool | ()) -> Bool
 Bool + Bool * (Bool | ()) -> Bool
 2^(2 + 2 * (2 + 1))
 2^8
 256
-}


-- | 1.4i

-- Prove: (a^b)^c = a^b*c

{-
  (c -> b -> a) -> (c,b) -> a = uncurry
  (b,c) -> a -> (c -> b -> a) = curry
-}

-- | 1.4ii

-- Prove: a^b * a^c = a^b+c

expLaws :: (b -> a, c -> a) -> Either b c -> a
expLaws fs (Left b) = fst fs b
expLaws fs (Right c) = snd fs c

expLaws' :: Either b c -> a -> (b -> a, c -> a)
expLaws' _ a = (const a, const a)

expLawsAlt :: (b -> a, c -> a) -> (Either b c -> a)
expLawsAlt fs = \x -> case x of
                        Left b  -> fst fs b
                        Right c -> snd fs c

expLawsAlt' :: (Either b c -> a) -> (b -> a, c -> a)
expLawsAlt' e = (e . Left, e . Right)

-- | 1.4iii

-- Prove: (a * b)^c = a^c * b^c

expLaws2  :: ( c -> (a, b)) -> (c -> a, c -> b)
expLaws2 f = (fst . f, snd . f)

expLaws2' :: (c -> a, c -> b) -> ( c -> (a, b))
expLaws2' (f, g) = \c -> (f c, g c)


