{-# LANGUAGE ExistentialQuantification #-}

module BinRep where

import Control.Monad
import Control.Applicative
import Data.Traversable
import Data.Foldable hiding (elem, all, foldl, concat, sum)
import Data.Maybe
import Data.Monoid
import Data.Char
import Data.List
-- import Network.CGI
import Text.XHtml hiding (tag)
import Numeric

import QuizUtils
import Sig
import HackOldQuiz

p2p :: Mo [Int]
p2p = do
  xs <- take 6 <$> jumble [2..16]
  jumble (0 : 1 : xs)

pow :: Int -> Int   -- I know, I should do it the quick way
pow 0 = 1
pow n = 2 * pow (n - 1)

expp :: Int -> Mo Html
expp n = do
  (_, h, m) <- numBox ("exp" ++ show n) (Just (pow n))
  return  $    td << [id << "2", sup << show n, id << " = ", primHtml h]
          +++  td << [primHtml m, primHtml "&nbsp; &nbsp; "]

logp :: Int -> Mo Html
logp n = do
  (_, h, m) <- numBox ("log" ++ show n) (Just n)
  return  $    td << [id << show (pow n), id << " = 2", sup << primHtml h]
          +++  td << [primHtml m, primHtml "&nbsp; &nbsp; "]

parta :: Mo Html
parta = do
  ld <- traverse expp =<< p2p
  rd <- traverse logp =<< p2p
  return $ table << zipWith (\ l r -> tr << [l, r]) ld rd

po2s :: Int -> [Int]
po2s 0 = []
po2s n = let (q, r) = divMod n 2 in map (2*) (po2s q) ++ case r of
  1 -> [1]
  0 -> []

isp :: Int -> Bool
isp 1 = True
isp n = case divMod n 2 of
  (_, 1) -> False
  (n, 0) -> isp n

bins :: Int -> String -> ES Int
bins a ('0' : s) | all isSpace s = return (a * 2)
                 | otherwise     = bins (a * 2) s
bins a ('1' : s) | all isSpace s = return (a * 2 + 1)
                 | otherwise     = bins (a * 2 + 1) s
bins a (w : s) | isSpace w = bins a s
bins a _ = Err "Binary?"

hexc :: Char -> ES Int
hexc '0' = return 0
hexc '1' = return 1
hexc '2' = return 2
hexc '3' = return 3
hexc '4' = return 4
hexc '5' = return 5
hexc '6' = return 6
hexc '7' = return 7
hexc '8' = return 8
hexc '9' = return 9
hexc 'A' = return 10
hexc 'B' = return 11
hexc 'C' = return 12
hexc 'D' = return 13
hexc 'E' = return 14
hexc 'F' = return 15
hexc 'a' = return 10
hexc 'b' = return 11
hexc 'c' = return 12
hexc 'd' = return 13
hexc 'e' = return 14
hexc 'f' = return 15
hexc c = Err (c : " is not a hexadecimal digit!")

hexs :: Int -> String -> ES Int
hexs a (w : s) | isSpace w = bins a s
hexs a (c : s) = do
  h <- hexc c
  let b = a * 16 + h
  if all isSpace s then return b else hexs b s
hexs a [] = Err "Hexadecimal?"

psum :: [String] -> ES [Int]
psum (x : xs) = case reads x of
  [(i, "")]
    | isp i -> case xs of
        [] -> return [i]
        ("+" : ys) -> do
          is <- psum ys
          if elem i is then Err (show i ++ "used twice!")
            else return (i : is)
        _ -> Err "Not a sum!"
    | otherwise -> Err (show i ++ " not a power of 2!")
  _ -> Err (x ++ " not a number!")
psum [] = Err "More here?"

dpbhp :: Int -> (Int, Int) -> Mo Html
dpbhp i r = do
  let si = show i
  n <- mo (Rnd r)
  (ps, ph) <- qfield ("dpbhp" ++ si) "" [size "40"]
  pm <- case psum (myLex ps) of
    Err s -> abuse << s
    OK is
      | sort is == sort (po2s n) -> qpraise << "OK"
      | otherwise -> abuse << ["That sums to ", show (sum is), "!"]
  (bs, bh) <- qfield ("dpbhb" ++ si) "" [size "16"]
  bm <- case bins 0 bs of
    Err s -> abuse << s
    OK b | b == n -> qpraise << "OK"
         | otherwise -> abuse << "Wrong number!"
  (hs, hh) <- qfield ("dpbhh" ++ si) "" [size "4"]
  hm <- case hexs 0 hs of
    Err s -> abuse << s
    OK h | h == n -> qpraise << "OK"
         | otherwise -> abuse << "Wrong number!"
  return $
    tr << [td << [show n], td << [id << " = ", ph], 
           td << [id << " = ", bh, sub << "bin"],
           td << [id << " = ", hh, sub << "hex"]]
    +++
    tr << [td noHtml, td pm, td bm, td hm]

inhex :: Int -> String
inhex 0 = ""
inhex n = case (divMod n 16) of
  (q, r) -> inhex q ++ ["0123456789ABCDEF" !! r]

hbpdp :: Int -> (Int, Int) -> Mo Html
hbpdp i r = do
  let si = show i
  n <- mo (Rnd r)
  (bs, bh) <- qfield ("hbpdb" ++ si) "" [size "16"]
  bm <- case bins 0 bs of
    Err s -> abuse << s
    OK b | b == n -> qpraise << "OK"
         | otherwise -> abuse << "Wrong number!"
  (ps, ph) <- qfield ("hbpdp" ++ si) "" [size "40"]
  pm <- case psum (myLex ps) of
    Err s -> abuse << s
    OK is
      | sort is == sort (po2s n) -> qpraise << "OK"
      | otherwise -> abuse << ["Not the right powers!"]
  (ds, dh) <- qfield ("hbpdd" ++ si) "" [size "6"]
  dm <- case readDec ds of
    [(d, s)] | all isSpace s ->
      if d == n then qpraise << "OK" else abuse << "Wrong number!"
    _ -> abuse << "Decimal?"
  return $
    tr << [  td << [id << inhex n,sub << "hex"],
             td << [id << " = ", bh, sub << "bin"], 
             td << [id << " = ", ph],
             td << [id << " = ", dh]]
    +++
    tr << [td noHtml, td bm, td pm, td dm]

ddbdt :: Int -> Maybe Int -> Mo Html
ddbdt 0 _ = return noHtml
ddbdt i n = do
  let s = show i
  (n', qh, qm) <- numBox ("ddbdq" ++ s) (flip div 2 <$> n)
  (_ , rh, rm) <- numBox ("ddbdr" ++ s) (flip mod 2 <$> n)
  (tr << [td << "2 * ", td << [primHtml qh, primHtml qm],
    td << [id << " + ",primHtml rh,primHtml rm]] +++) <$>
    ddbdt (i - 1) n'

binRepPartA :: Mo String
binRepPartA = boxForm "parta" "Powers of Two" =<<
  (renderHtml <$> parta)

binRepPartB :: Mo String
binRepPartB = boxForm "partb" "Decimal to Binary to Hexadecimal" =<< do
  b1 <- dpbhp 1 (4, 15)
  b2 <- dpbhp 2 (16, 255)
  b3 <- dpbhp 3 (256, 65535)
  return $ concat
    [  tag "p" "" $ concat
       [  "Express each of the following decimal numbers as a sum "
       ,  "of distinct powers of 2. Then read off its binary "
       ,  "representation, "
       ,  "and finally, convert it to hexadecimal."
       ]
    ,  tag "p" "" $ concat
       [  tag "strong" "" "Example: "
       ,  "42 = 32 + 8 + 2 = 101010", tag "sub" "" "bin"
       ,  " = 2A", tag "sub" "" "hex"
       ]
    ,  renderHtml (table << [b1, b2, b3])
    ]

{-
guff :: Mo Html
guff = do
  b1 <- dpbhp 1 (4, 15)
  b2 <- dpbhp 2 (16, 255)
  b3 <- dpbhp 3 (256, 65535)
  c1 <- hbpdp 1 (4, 15)
  c2 <- hbpdp 2 (16, 255)
  c3 <- hbpdp 3 (256, 65535)
  dn <- mo (Rnd (8192, 65535))
  dt <- ((tr << [td noHtml, td << strong << show dn, td noHtml]) +++) <$>
        ddbdt 16 (Just dn)
  (ds, dh) <- qfield "ddbh" "" [size "4"]
  dm <- case hexs 0 ds of
    Err s -> abuse << s
    OK h | h == dn -> praise << "OK"
         | otherwise -> abuse << "Wrong number!"
  (sb, paf) <- qsub
  return $ fold
    [  qpart ceQ "parta" "(a) Powers of Two" <<
       [  pah
       ,  sb
       ]
    ,  qpart ceQ "partb" "(b) Decimal to Binary to Hexadecimal" <<
       [  paragraph <<
          [  id << "Express each of the following decimal numbers as a sum "
          ,  id << "of distinct powers of 2. Then read off its binary "
          ,  id << "representation, "
          ,  id << "and finally, convert it to hexadecimal."
          ]
       ,  paragraph <<
          [  strong << "Example: "
          ,  id << "42 = 32 + 8 + 2 = 101010", sub << "bin"
          ,  id << " = 2A", sub << "hex"
          ]
       ,  table << [b1, b2, b3]
       ,  sb
       ]
    ]
-}

binRepPartC :: Mo String
binRepPartC = boxForm "partc" "Hexadecimal to Binary to Decimal" =<< do
  c1 <- hbpdp 1 (4, 15)
  c2 <- hbpdp 2 (16, 255)
  c3 <- hbpdp 3 (256, 65535)
  return . renderHtml <<
    [  paragraph <<
       [  id << "Convert each of the following hexadecimal numbers "
       ,  id << "to binary. Then express it as a sum of "
       ,  id << "distinct powers of 2. Then add up the powers "
       ,  id << "to get its value in decimal."
       ]
    ,  table << [c1, c2, c3]
    ]

binRepPartD :: Mo String
binRepPartD = boxForm "partd" "Repeated Division" =<< do
  dn <- mo (Rnd (8192, 65535))
  dt <- ((tr << [td noHtml, td << strong << show dn, td noHtml]) +++) <$>
        ddbdt 16 (Just dn)
  (ds, dh) <- qfield "ddbh" "" [size "4"]
  dm <- case hexs 0 ds of
    Err s -> abuse << s
    OK h | h == dn -> qpraise << "OK"
         | otherwise -> abuse << "Wrong number!"
  return . renderHtml <<
    [  table << tr ! [valign "top"] <<
       [  td ! [width "250"] << table << dt
       ,  td << paragraph <<
          [  id << "Another way to convert to binary is to keep "
          ,  id << "on dividing by 2 and reading off the remainders "
          ,  id << "to get the bits from right to left."
          ,  br , br
          ,  id << "Example:"
          ,  table <<
             [ tr << [  td ! [width "40"] << noHtml,
                        td noHtml, td << "14", td noHtml]
             , tr << map (td <<) ["14 = ", "2 * ", "7", " + 0"]
             , tr << map (td <<) ["7 = ",  "2 * ", "3", " + 1"]
             , tr << map (td <<) ["3 = ",  "2 * ", "1", " + 1"]
             , tr << map (td <<) ["1 = ",  "2 * ", "0", " + 1"]
             ]
          ,  br, br
          ,  id << "So that makes 14 = 1110", sub << "bin"
          ,  br, br
          ,  id << "You get a bigger number to play with, of course!"
          ]
       ]
    ,  paragraph <<
         [  id << "Now tilt your head leftward, and tell me "
         ,  id << "what ", id << show dn
         ,  id << " is in hexadecimal: "
         ,  dh, sub << "hex", dm
         ]
    ]


