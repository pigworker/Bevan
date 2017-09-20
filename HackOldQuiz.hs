module HackOldQuiz where

import Control.Monad
import Control.Applicative
import Text.XHtml hiding (tag)
import Data.Char

import QuizUtils
import Sig

data ES x = Err String | OK x deriving Show

instance Monad ES where
  return = OK
  OK a >>= f = f a
  Err s >>= f = Err s
  fail = Err

instance Applicative ES where
  pure = return
  (<*>) = ap

instance Functor ES where
  fmap = ap . return

solo :: String
solo = "()[]{},;./\\|^%_"

myLex :: String -> [String]
myLex [] = []
myLex (x : xs)
  | isSpace x = myLex xs
  | isAlpha x =
    let (ys, zs) = span isAlphaNum xs in (x : ys) : myLex zs
  | isDigit x =
    let (ys, zs) = span isDigit xs in (x : ys) : myLex zs
  | elem x solo = [x] : myLex xs
  | otherwise =
    let (ys, zs) =
         span (\ c -> not (isSpace c || isAlphaNum c || elem c solo)) xs
    in  (x : ys) : myLex zs

qpraise :: Html -> Mo Html
qpraise h = return (thespan ! [thestyle "color:blue"] << h)

abuse :: Html -> Mo Html
abuse h = damn "" >> return (thespan ! [thestyle "color:red"] << h)

qfield :: String -> String -> [HtmlAttr] -> Mo (String, Html)
qfield k d as = do
  u <- mo (Var k)
  return $ (u, input ! [name k, value u] ! as)

