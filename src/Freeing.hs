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

newtype Command1 m a = Command1 {
  unCommand1 :: FreeT CountF m a
} deriving (Functor, Applicative, Monad)

runCommand1 :: Monad m => (CountF (m a) -> m a) -> Command1 m a -> m a
runCommand1 countAlg command = iterT countAlg $ unCommand1 command

instance Monad m => MonadCount (Command1 m) where
  inc = Command1 (liftF (Inc ()))
  get = Command1 (liftF (Get id))
