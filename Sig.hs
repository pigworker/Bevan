{-# LANGUAGE DeriveFunctor, GADTs, RankNTypes, KindSignatures #-}

module Sig where

import Control.Applicative
import Control.Monad
import Data.Monoid
import System.Random

import Events
import Storage

data Sig :: * -> * where
  Var :: String -> Sig String
  VarChange :: String -> Sig Bool
  VarDef :: String -> Sig Bool
  PTitle :: Sig String
  VTime :: Sig String
  EventListings :: Sig [Event]
  EventUpdates :: Sig [PageUpdate]
  Damn :: Sig ()
  Damned :: Sig Bool
  Pass :: Sig ()
  Passed :: String -> Sig Bool
  Score :: Int -> Sig ()
  Rnd :: Random a => (a, a) -> Sig a
  Groups :: Sig [String]
  DScore :: Sig (Int, Int)
  Tout :: Sig String
  View :: Sig View
  Alert :: Sig ()
  Dump :: Sig String

data View
  = Staff
  | Student
  | Snoop String -- username of snoopee
  deriving Show

data Mo :: * -> * where
  Ret  :: x -> Mo x
  (:?) :: Sig r -> (forall t. (x -> Mo t) -> r -> Mo t) -> Mo x

instance Monad Mo where
  return = Ret
  Ret x >>= f = f x
  (c :? g) >>= f = c :? \ k r -> g ((>>= k) . f) r

(??) :: Sig r -> (r -> Mo x) -> Mo x
c ?? f = c :? \ k r -> f r >>= k

mo :: Sig r -> Mo r
mo c = c ?? Ret

instance Applicative Mo where
  pure = return
  (<*>) = ap

instance Functor Mo where
  fmap = (<*>) . pure

instance Monoid x => Monoid (Mo x) where
  mempty = pure mempty
  mappend a b = mappend <$> a <*> b

