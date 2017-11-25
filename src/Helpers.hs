module Helpers where

import Control.Monad

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (h:t) = Just h