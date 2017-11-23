module Helpers where

import Control.Monad

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return
