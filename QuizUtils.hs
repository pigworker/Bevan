{-# LANGUAGE KindSignatures, GADTs, DataKinds, PatternGuards #-}

module QuizUtils where

import Control.Applicative
import Data.Char
import Data.Monoid
import Data.List

import Sig
import ExParse


tag :: String -> String -> String -> String
tag t a c = concat ["<", t, if null a then "" else ' ' : a, ">", c, "</", t, ">"]

attrsL :: [(String, String)] -> String
attrsL = intercalate " " . map (\ (k, v) -> k ++ "=\"" ++ v ++ "\"")

textArea :: String -> Int -> Int -> String -> Mo (String, String)
textArea nom rows cols init = do
  tv <- mo (Var nom)
  let v = if null tv then init else tv
  return (v, tag "textarea" 
    (concat ["name=", show nom, " rows=", show (show rows),
            " cols=", show (show cols), " style=\"font-family:monospace\""])
    v)

textField :: String -> Int -> Mo (String, String)
textField nom size = do
  v <- mo $ Var nom
  return $ (v, tag "input"
    (concat ["name=", show nom, " value=", show v,
             " type=\"text\" size=\"", show size,
             "\" maxlength=\"", show size, "\""])
    "")

numBox :: String -> Maybe Int -> Mo (Maybe Int, String, String)
numBox nom Nothing = do
  w <- damn "Wait"
  return (Nothing, tag "input"
    (concat ["name=", show nom, " type=\"text\" size=\"5\""])
    "", w)
numBox nom (Just i) = do
  v <- mo (Var nom)
  let h = tag "input"
        (concat ["name=", show nom, " value= ", show v, " type=\"text\" "
                ,"size=\"", show (length (show i)), "\""]) ""
  case (all isSpace v, reads v :: [(Int, String)]) of
    (True, _) -> do
      d <- damn "Ready"
      return (Nothing, h, d)
    (False, [(j, r)]) | all isSpace r -> do
      v <- if i == j then praise "OK" else damn "No"
      return (Just j, h, v)
    (False, _) -> do
      v <- damn "Number?"
      return (Nothing, h, v)

praise :: String -> Mo String
praise s = return $ tag "span" "style=\"color:blue\"" s

worry :: String -> Mo String
worry s = return $ tag "span" "style=\"color:green\"" s

damn :: String -> Mo String
damn m = do
  mo Damn
  return $ tag "span" "style=\"color:red\"" m

splat :: [x] -> Int -> (x, [x])
splat (x : xs) 0 = (x, xs)
splat (x : xs) i = (y, x : ys) where (y, ys) = splat xs (i - 1)

jumble :: [x] -> Mo [x]
jumble xs = help (length xs) xs where
  help 0 xs = pure xs
  help i xs = do
    (y, ys) <- splat xs <$> mo (Rnd (0, i - 1))
    (y :) <$> help (i - 1) ys

radioButtons :: String -> [(String, String, String)] -> (String -> Bool) -> Mo String
radioButtons nom opts chk = do
  cv <- mo (Var nom)
  msg <- if chk cv then return "Thanks.<br/>" else do
    damn "NOTE: Not done yet.<br/>"
  return $ concat
    [  tag "input"
        (concat ["type=\"radio\" name=", show nom, " value=", show v,
                 if v == cv then " checked" else ""])
        (l ++ (if v == cv then c else "") ++ "<br/>")
    |  (v, l, c) <- opts
    ] ++ msg

boxForm :: String -> String -> String -> Mo String
boxForm l n c = do
  return $
    tag "a" ("id=" ++ show l) $
    tag "form" ("method=\"post\" action=\"#" ++ l ++ "\"") $
    tag "fieldset" "" $ concat
      [  tag "legend" "" n
      ,  c
      ,  tag "input" ("type=\"submit\" value=\"Send\"") ""
      ]

syntaxCheck :: ExParse Char x -> String -> Mo (Either String x)
syntaxCheck p s = case exParse (p <* many (eat isSpace) <* eoi) s of
  (_, Right x, _) -> return (Right x)
  (b, Left d, a) -> Left <$> mconcat
    [  damn "Syntax error:"
    ,  return " I read as far as<br/>"
    ,  return . tag "span" "style=\"color:blue\"" $ tag "pre" "" b
    ,  return "<br/>"
    ,  return $ case d of
         Clueless -> "and then I got confused, before<br/>"
         Thing k "" -> "and then I couldn't find " ++ k ++ " at<br/>"
         Thing k p -> "and then I wanted " ++ tag "pre" "" p ++
                        " to begin " ++ k ++ " at<br/>"
    ,  return $
         if all isSpace b
           then "the end of your input."
           else tag "span" "style=\"color:red\"" $ tag "pre" "" a
    ,  return "<br/>So have another go, or ask for help.<p/>"
    ]
