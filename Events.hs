module Events where

import Data.List
--import System.Locale
import Data.Time

import Dent

data Event = Event
  {  evKind :: String
  ,  evNumber :: String
  ,  evTitle :: String
  ,  evTime :: String
  ,  evPlace :: String
  ,  evDuration :: String
  ,  evOwner :: String
  ,  evGroups :: [String]
  ,  evNodes :: [String]
  }  deriving Show

idT :: LocalTime -> LocalTime
idT = id

evTimeText :: Event -> String
evTimeText e = case parseTimeM True defaultTimeLocale "%y-%m-%d-%H-%M" (evTime e) of
  Just d -> formatTime defaultTimeLocale "on %A %e %B at %H:%M" (idT d)
  Nothing -> "at a time unknown"

fish :: String -> Dent -> [String]
fish k Blank = []
fish k ((_, s) :- _) = case stripPrefix k s of
  Just t -> words t
  _ -> []

hwd :: a -> [a] -> a
hwd a [] = a
hwd _ (a : _) = a

evDent :: Dent -> [Event]
evDent Blank = []
evDent ((_, t) :- ds) = case (words t, ds >>= fish "when") of
  (k : n : ts, w : _) -> [ Event
    {  evKind = k
    ,  evNumber = n
    ,  evTitle = unwords ts
    ,  evTime = w
    ,  evPlace = unwords (ds >>= fish "where")
    ,  evDuration = unwords (ds >>= fish "howlong")
    ,  evOwner = unwords (ds >>= fish "whose")
    ,  evGroups = ds >>= fish "groups"
    ,  evNodes = ds >>= fish "stuff"
    }]
  _ -> []
