{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE TemplateHaskell #-}

module Freeing where

import Control.Lens
import Control.Monad (join)
import Control.Monad.State.Strict
import Control.Monad.Trans.Class
import Control.Monad.Trans.Free
import Data.Functor.Sum

-- Generic Algebra / Natural Transformation boilerplate

type Algebra f a = f a -> a
type NatTrans f m = forall a. f a -> m a

natTransAlgebra :: Monad m => NatTrans f m -> Algebra f (m a)
natTransAlgebra nt fma = join $ nt fma

iterNT :: (Functor f, Monad m) => NatTrans f m -> FreeT f m a -> m a
iterNT nt free = iterT (natTransAlgebra nt) free

-- Free functors

data CountF a = IncCount a | GetCount (Int -> a) deriving (Functor)

data ConsoleF a = TellConsole String a | AskConsole (String -> a) deriving (Functor)

-- Classes for our free functors

class Monad m => MonadCount m where
  incCount :: m ()
  getCount :: m Int

class Monad m => MonadConsole m where
  tellConsole :: String -> m ()
  askConsole :: m String

-- A simple MonadCount

newtype CountCommand m a = CountCommand {
  unCountCommand :: FreeT CountF m a
} deriving (Functor, Applicative, Monad)

runCountCommand :: Monad m => NatTrans CountF m -> CountCommand m a -> m a
runCountCommand nt command = iterNT nt $ unCountCommand command

instance Monad m => MonadCount (CountCommand m) where
  incCount = CountCommand (liftF (IncCount ()))
  getCount = CountCommand (liftF (GetCount id))

-- A simple MonadConsole

newtype ConsoleCommand m a = ConsoleCommand {
  unConsoleCommand :: FreeT ConsoleF m a
} deriving (Functor, Applicative, Monad)

runConsoleCommand :: Monad m => NatTrans ConsoleF m -> ConsoleCommand m a -> m a
runConsoleCommand nt command = iterNT nt $ unConsoleCommand command

instance Monad m => MonadConsole (ConsoleCommand m) where
  tellConsole s = ConsoleCommand (liftF (TellConsole s ()))
  askConsole = ConsoleCommand (liftF (AskConsole id))

-- Working with the Sum of the two Functors

newtype SF a = SF { unSF :: Sum CountF ConsoleF a } deriving (Functor)

_CountF :: Prism' (SF a) (CountF a)
_CountF = prism' setCountF getCountF
  where
    setCountF = SF . InL
    getCountF (SF (InL f)) = Just f
    getCountF _ = Nothing

_ConsoleF :: Prism' (SF a) (ConsoleF a)
_ConsoleF = prism' setConsoleF getConsoleF
  where
    setConsoleF = SF . InR
    getConsoleF (SF (InR f)) = Just f
    getConsoleF _ = Nothing

data SFTrans m = SFTrans
  { _sfTransCount :: NatTrans CountF m
  , _sfTransConsole :: NatTrans ConsoleF m
  }

-- Be careful you really match a completely with this
-- the `total` lib does this, but I couldn't get it to work
total :: [(a -> Maybe b)] -> a -> b
total [] _ = error "non-exhaustive"
total (f:fs) a =
  case f a of
    Nothing -> total fs a
    Just b -> b

mkSFNatTrans :: SFTrans m -> NatTrans SF m
mkSFNatTrans (SFTrans count console) =
  total
    [ \f -> count <$> preview _CountF f
    , \f -> console <$> preview _ConsoleF f
    ]

newtype Command m a = Command {
  unCommand :: FreeT SF m a
} deriving (Functor, Applicative, Monad, MonadTrans)

runCommand :: Monad m => NatTrans SF m -> Command m a -> m a
runCommand nt command = iterNT nt $ unCommand command

setSF :: Monad m => SF a -> Command m a
setSF = Command . liftF

instance Monad m => MonadCount (Command m) where
  incCount = setSF . review _CountF  $ IncCount ()
  getCount = setSF . review _CountF $ GetCount id

instance Monad m => MonadConsole (Command m) where
  tellConsole s = setSF . review _ConsoleF $ TellConsole s ()
  askConsole = setSF . review _ConsoleF $ AskConsole id

-- An example of interleaving the two Functors

example :: Monad m => Command m ()
example = do
  tellConsole "guess the number"
  incCount
  guess <- askConsole
  actual <- getCount
  if (read guess) == actual
    then tellConsole "right!"
    else tellConsole $ "wrong, it was " ++ show actual

-- A concrete interpretation of our effects

newtype Concrete a = Concrete
  { unConcrete :: StateT Int IO a
  } deriving (Functor, Applicative, Monad)

countConcrete :: NatTrans CountF Concrete
countConcrete (IncCount a) = Concrete $ do
  i <- get
  put (i + 1)
  return a
countConcrete (GetCount f) = Concrete $ f <$> get

consoleConcrete :: NatTrans ConsoleF Concrete
consoleConcrete (TellConsole s a) = Concrete $ do
  liftIO $ putStrLn s
  return a
consoleConcrete (AskConsole f) = Concrete $ f <$> (liftIO getLine)

vConcrete :: SFTrans Concrete
vConcrete = SFTrans countConcrete consoleConcrete

sfConcrete :: NatTrans SF Concrete
sfConcrete = mkSFNatTrans vConcrete

runExample :: IO ()
runExample = runStateT (unConcrete (runCommand sfConcrete example)) 0 >> return ()
