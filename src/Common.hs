module Common where

import Control.Monad.Trans.State
import Control.Monad.Trans.Maybe
import Control.Monad
import Models

-- A StateT stack is used so that we can easily update and get robot state without explicitly passing it around
-- MaybeT is used becuase when reading and parsing a command, we may or may not get a valid command
type RobotState = StateT Robot (MaybeT IO) ()

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

headMaybe :: [a] -> Maybe a
headMaybe [] = Nothing
headMaybe (h:t) = Just h
