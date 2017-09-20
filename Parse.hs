module Parse where

import Data.Char
import Control.Applicative
import Control.Monad
import Data.Monoid
import Data.Traversable

newtype Parse t x = Parse {parse :: [t] -> Maybe (x, [t])}
instance Monad (Parse t) where
  return x = Parse $ \ s -> Just (x, s)
  Parse f >>= k = Parse $ \ s -> do
    (x, s) <- f s
    parse (k x) s
instance Applicative (Parse t) where
  pure = return
  (<*>) = ap
instance Functor (Parse t) where
  fmap = (<*>) . pure
instance Alternative (Parse t) where
  empty = Parse $ \ _ -> Nothing
  Parse f <|> Parse g = Parse $ \ s -> f s <|> g s
instance MonadPlus (Parse t) where
  mzero = empty
  mplus = (<|>)
instance Monoid (Parse t x) where
  mempty = empty
  mappend = (<|>)

ch :: (t -> Bool) -> Parse t t
ch p = Parse $ \ s -> case s of
  c : s | p c -> Just (c, s)
  _ -> Nothing

eoi :: Parse t ()
eoi = Parse $ \ s -> if null s then Just ((), s) else Nothing

roi :: Parse t [t]
roi = Parse $ \ s -> Just (s, [])

eat :: Eq t => [t] -> Parse t ()
eat s = () <$ traverse (ch . (==)) s

punc :: String -> Parse Char ()
punc s = () <$ spc <* eat s <* spc

pOne :: Parse t x -> Parse [t] x
pOne p = Parse $ \ tss -> case tss of
  ts : tss -> do
    (x, []) <- parse p ts
    return (x, tss)
  _ -> Nothing

peek :: Parse t () -> Parse t ()
peek p = Parse $ \ s -> ((), s) <$ parse p s

untilp :: Parse t () -> Parse t x -> Parse t [x]
untilp n x = [] <$ n <|> (:) <$> x <*> untilp n x

spc :: Parse Char ()
spc = () <$ many (ch isSpace)
