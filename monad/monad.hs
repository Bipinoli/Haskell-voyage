-- maybeAdd (Just 2) 2 = Just 4
-- maybeAdd Nothing 2 = Nothing
maybeAdd :: (Num a) => Maybe a -> a -> Maybe a
maybeAdd mx y = mx >>= (\x -> Just $ x + y)

-- Two monadic addition
-- maybeAdd2 (Just 3) (Just 5) = Just 8
-- maybeAdd2 (Just 2) Nothing = Nothing
maybeAdd2 :: (Num a) => Maybe a -> Maybe a -> Maybe a
maybeAdd2 mx my = mx >>= (\x -> my >>= (\y -> Just $ x + y))

-- More general moadic addition
monadd :: (Monad m, Num n) => m n -> m n -> m n
monadd mx my = mx >>= (\x -> my >>= (\y -> return $ x + y))

-- Using do notation
monadd2 :: (Monad m, Num n) => m n -> m n -> m n
monadd2 mx my = do
  x <- mx
  y <- my
  return $ x + y

-- My custom Maybe Monad
-- instance Monad MyMaybe where
--   m >>= f = case m of
--     Nothing -> Nothing
--     Just x -> f x
--   return v = Just v
