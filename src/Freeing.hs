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

newtype Command m a = Command {
  unCommand :: FreeT CountF m a
} deriving (Functor, Applicative, Monad)

runCommand :: Monad m => (CountF (m a) -> m a) -> Command m a -> m a
runCommand countAlg command = iterT countAlg $ unCommand command

instance Monad m => MonadCount (Command m) where
  inc = Command (liftF (Inc ()))
  get = Command (liftF (Get id))
