module Dent where

import Data.Char
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as B8

data Dent = (Int, String) :- [Dent] | Blank deriving Show

dents :: Int -> [B.ByteString] -> ([Dent], [B.ByteString])
dents i [] = ([], [])
dents i (s : ss) = case B8.span isSpace s of
  (b, t) | B8.null t -> let (ds, ts) = dents i ss in (Blank : ds, ts)
         | otherwise -> case fromIntegral (B8.length b) of
    j | j > i ->
      let  (ds, ts) = dents j ss
           (es, us) = dents i ts
      in   (((j - i, B8.unpack t) :- ds) : es, us)
    _ -> ([], s : ss)

dentify :: B.ByteString -> [Dent]
dentify = fst . dents (negate 1) . B8.lines

stned :: Int -> Dent -> [B.ByteString]
stned i Blank = [B.empty]
stned i ((j , t) :- ds) =
  let k = i + j
  in  (B8.concat[B8.replicate (fromIntegral k) ' ', B8.pack t])
        : (ds >>= stned k)

yfitned :: [Dent] -> B.ByteString
yfitned ds = B8.unlines (ds >>= stned (negate 1))

dHead :: Dent -> String
dHead ((_, s) :- _) = s
dHead Blank = ""
