{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Freeing where

import Control.Monad.Trans.Free
import Data.Functor.Sum

data CountF a = Inc a | Get (Int -> a) deriving (Functor)

data ConsoleF a = Tell String a | Ask (String -> a) deriving (Functor)

class Monad m => MonadCount m where
  inc :: m ()
  get :: m Int

class Monad m => MonadConsole m where
  tell :: String -> m ()
  ask :: m String

newtype CountCommand m a = CountCommand {
  unCountCommand :: FreeT CountF m a
} deriving (Functor, Applicative, Monad)

runCountCommand :: Monad m => (CountF (m a) -> m a) -> CountCommand m a -> m a
runCountCommand countAlg command = iterT countAlg $ unCountCommand command

instance Monad m => MonadCount (CountCommand m) where
  inc = CountCommand (liftF (Inc ()))
  get = CountCommand (liftF (Get id))

newtype ConsoleCommand m a = ConsoleCommand {
  unConsoleCommand :: FreeT ConsoleF m a
} deriving (Functor, Applicative, Monad)

runConsoleCommand :: Monad m => (ConsoleF (m a) -> m a) -> ConsoleCommand m a -> m a
runConsoleCommand consoleAlg command = iterT consoleAlg $ unConsoleCommand command

instance Monad m => MonadConsole (ConsoleCommand m) where
  tell s = ConsoleCommand (liftF (Tell s ()))
  ask = ConsoleCommand (liftF (Ask id))


