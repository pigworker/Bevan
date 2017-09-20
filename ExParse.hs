module ExParse where

import Control.Monad
import Control.Applicative
import Data.Monoid
import Data.Maybe
import Data.Traversable
import Data.Char

newtype ExParse t x =
  ExParse {exParse :: [t] -> ([t], Either (Diagnostic t) x, [t])}

data Diagnostic t
  = Clueless
  | Thing String [t]
  deriving Show

instance Monoid (Diagnostic t) where
  mempty = Clueless
  mappend Clueless d = d
  mappend d Clueless = d
  mappend ats@(Thing a ts) bus@(Thing b us) =
    case compare (length ts) (length us) of
      LT -> ats
      GT -> bus
      EQ -> Thing (a ++ " or " ++ b) ts -- which should be the same as us

instance Monad (ExParse t) where
  return x = ExParse $ \ ts -> ([], Right x, ts)
  p >>= k = ExParse $ \ ts -> case exParse p ts of
    (as, Left d, ts) -> (as, Left d, ts)
    (as, Right x, ts) -> case exParse (k x) ts of
      (bs, y, ts) -> (as ++ bs, y, ts)

instance Monoid (ExParse t x) where
  mempty = ExParse $ \ ts -> ([], Left Clueless, ts)
  mappend p p' = ExParse $ \ ts -> case exParse p ts of
    y@(_, Right _, _) -> y
    n@(as, Left d, us) -> case exParse p' ts of
      y@(_, Right _, _) -> y
      n'@(bs, Left d', vs) -> case compare (length as) (length bs) of
        GT -> n
        LT -> n'
        EQ -> (as, Left (mappend d d'), vs)

($?) :: String -> ExParse t x -> ExParse t x
s $? p = ExParse $ \ ts -> case exParse p ts of
  y@(_, Right _, _) -> y
  n@(as, Left Clueless, ts) -> (as, Left (Thing s as), ts)
  n -> n

instance Functor (ExParse t) where fmap = liftM
instance Applicative (ExParse t) where pure = return ; (<*>) = ap
instance MonadPlus (ExParse t) where mzero = mempty ; mplus = mappend
instance Alternative (ExParse t) where empty = mempty ; (<|>) = mappend

eat :: (t -> Bool) -> ExParse t t
eat f = ExParse $ \ ts -> case ts of
  t : ts | f t -> ([t], Right t, ts)
  ts -> ([], Left Clueless, ts)

eoi :: ExParse t ()
eoi = ExParse $ \ ts -> case ts of
  [] -> ([], Right (), [])
  ts -> ([], Left Clueless, ts)

mayp :: Maybe x -> ExParse t x
mayp = maybe empty return

toks :: Eq t => [t] -> ExParse t ()
toks ts = () <$ traverse (eat . (==)) ts

-- like toks, but doesn't care about spacing
toksSpcs :: String -> ExParse Char ()
toksSpcs [] = spc
toksSpcs (t:ts) | isSpace t = toksSpcs ts
                | otherwise = () <$ spc <* eat (==t) <* toksSpcs ts

opt :: ExParse t x -> ExParse t (Maybe x)
opt p = Just <$> p <|> pure Nothing

spc :: ExParse Char ()
spc = () <$ many (eat isSpace)

pInt :: ExParse Char Int
pInt = read <$> some (eat isDigit)
