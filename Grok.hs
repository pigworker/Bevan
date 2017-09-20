{-# LANGUAGE KindSignatures, GADTs, DataKinds, PatternGuards,
    TupleSections #-}

module Grok where

import Data.ByteString.Lazy.Char8 as B8 (unpack)
import Data.Char
import Data.List
import Control.Applicative
import Data.Traversable
import Control.Monad
import Data.Foldable hiding (concat, elem, all, maximum)
import Data.Monoid
import System.FilePath
import Data.Maybe
import Control.Arrow ((***))

-- import Cry -- not in the repo

import ExParse
import Dent
import Sig
import Events
import Storage
import QuizUtils
import HuffTree
import BinRep
import BinArith
import Q9
import TINYQuizKit
import CVIFront
import CVICompile (Glob, compile, basicGs)
import CVIParse (cviParse, tyShow, Type(..))
import LLL (oxford)
--import Scores

htmlc :: Char -> String
htmlc '<' = "&lt;"
htmlc '>' = "&gt;"
htmlc '&' = "&amp;"
htmlc c = [c]

grok :: Dent -> (Mo String, [Dent])
grok d@(_ :- ds) = case dHead d of
  '#' : _ -> (mempty, [d])
  ':' : s -> special (span isAlphaNum s) ds
  s -> mappend (fst (normal Top s), []) (foldMap grok ds)
grok Blank = (return "<p/>", [])

data VExp
  = String :$- [VExp]
  | VV String
  deriving Show

pVExp :: String -> ExParse Char VExp
pVExp pre = p where
  p = spc *>
    (VV <$> ((pre ++) <$> some (eat isAlphaNum))
    <|> (:$-) <$ eat (=='(')
             <*> some (eat isAlphaNum)
             <*> many p
             <* eat (==')'))

boov :: String -> Maybe Bool
boov "1" = Just True
boov "0" = Just False
boov _ = Nothing

eval :: (String -> Maybe t) -> [(String, [t] -> t)] -> VExp -> Mo (Maybe t)
eval vv ois (VV k) = do
  v <- mo (Var k)
  d <- mo (VarDef k)
  if d && not (null v) then return (vv v) else return Nothing
eval vv ois (f :$- es) = do
  vs <- traverse (eval vv ois) es
  case sequenceA vs of
    Nothing -> return Nothing
    Just vs -> case lookup f ois of
      Nothing -> return Nothing
      Just f -> return (Just (f vs))

yankId :: Dent -> (String, Dent)
yankId ((x, h) :- ds) = case span (not . isSpace) h of
  (i, s) -> (i, (x, s) :- ds)

data Stuff :: Bool -> * where
  Top :: Stuff True
  Until :: String -> Stuff False

ttags :: [(String, String)]
ttags =
  [  ("**", "strong")
  ,  ("*", "em")
  ,  ("__", "strong")
  ,  ("_", "em")
  ,  ("@@@", "h3")
  ,  ("@@", "h2")
  ,  ("@", "h1")
  ]

taggy :: String -> Maybe (Mo String, String)
taggy s = case [(t, f, s) | (t, f) <- ttags, s <- foldMap return (stripPrefix t s)] of
  [] -> Nothing
  ((t, f, s) : _) -> let (b, Until a) = normal (Until t) s in Just (tag f "" <$> b, a)

splitOn :: Eq x => x -> [x] -> [[x]]
splitOn p = unfoldr $ \ s -> case span (/= p) s of
  (a, _ : s)  -> Just (a, s)
  ([], [])    -> Nothing
  (a, [])     -> Just (a, [])

linkRel :: [String] -> [String] -> String
linkRel xs ys = go (reverse xs) ys where
  go xs [] = joinPath (reverse xs)
  go (_ : xs) (".." : ys) = go xs ys
  go [] (".." : ys) = go [] ys
  go xs (y : ys) = go (y : xs) ys

normal :: Stuff b -> String -> (Mo String, Stuff b)
normal (Until e) s | Just t <- stripPrefix e s = (return "", Until t)
normal Top        ""  = (return "\n", Top)
normal (Until _)  ""  = (return "\n", Until "")
normal tu ('\\':c:s) = let (s', t') = normal tu s in ((c:) <$> s', t')
normal tu s | Just (m, t) <- taggy s =
  let (n, u) = normal tu t in ((++) <$> m <*> n, u)
normal tu ('"':s) = ((('"':t) ++) <$> m, v)
  where
    stringy "" = ("\"", "")
    stringy ('\\':'"':s) = let (t, u) = stringy s in ("&#34;"++t, u)
    stringy ('"' : s) = ("\"", s)
    stringy (c : s) = let (t, u) = stringy s in (c : t, u)
    (t, u) = stringy s
    (m, v) = normal tu u
normal tu ('$' : s) = case span (\ c -> isAlphaNum c || elem c "_") s of
  (t, u) -> let (m, v) = normal tu u in ((++) <$> mo (Var t) <*> m, v)
normal tu ('[':'!':'=':s) = case span (not . (']'==)) s of
  (t, ']':u) -> let (m, v) = normal tu u in (("[NO GUEST IMAGE]" ++) <$> m, v)
  _ -> normal tu ""
normal tu ('[':'!':'!':'=':s) = case span (not . (']'==)) s of
  (t, ']':u) -> let (m, v) = normal tu u in (("[NO GUEST IMAGE]" ++) <$> m, v)
  _ -> normal tu ""
normal tu ('[':'>':s) = case span (not . ('='==)) s of
  (p, '=':u) ->
    let (m, Until v) = normal (Until "]") u
        (n, w) = normal tu v
    in  (, w) $ do
           pa <- splitOn '/' <$> mo (Var "node")
           sn <- mo View >>= \ x -> case x of
             Snoop x -> return ("&snoop=" ++ x)
             _       -> return ""
           txt <- m
           rest <- n
           return $ concat
             [  "<a href=\"https://personal.cis.strath.ac.uk/"
             ,  "conor.mcbride/Bevan?page="
             ,  linkRel pa (words p)
             ,  sn
             ,  "\">", txt, "</a>", rest
             ]
  _ -> normal tu ""
normal tu ('[':'!':s) = case span (not . (`elem` "]!")) s of
  (t, c:u) ->
    let (w, u') = case c of
          '!' -> case span (']'/=) u of
             (w, _:u') -> ("width=\"" ++ w ++ "\"", u')
             _ -> ("", u)
          ']' -> ("", u)
        (m, v) = normal tu u'
    in  (  (tag "img" (concat [w, "src=images/", t]) "" ++) <$> m
        ,  v)
  _ -> normal tu ""
normal tu ('[':'?':s) = case span (not . (`elem` "]")) s of
  (t, c:u) ->
    let (m, v) = normal tu u
        (nom, wid) = case words t of
          n : w : _ | all isDigit w -> (n, read w)
          n : _ -> (n, 16)
          _ -> ("fiddlesticks", 42)
    in  (, v) $ do
          ct <- mo $ Var "CONFIGtest"  -- yuk
          a <- case ct of
            "1" -> snd <$> textField nom wid
            _   -> (>>= htmlc) <$> (mo $ Var nom)
          (a ++) <$> m
  _ -> normal tu ""          
normal tu (c : s) = let (m, t) = normal tu s in ((c :) <$> m, t)
special :: (String, String) -> [Dent] -> (Mo String, [Dent])
special ("attendance", _) _ = (attendance, [])
special ("forthcoming", _) _ = (forthcoming, [])
special ("editAttendance", _) _ = (editAttendance, [])
special ("grok", _) _ = (grokBox, [])
special ("status", s) _ = (mv, []) where
  mv = do
    d <- mo Damned
    case (all isSpace s, d) of
      (True, False) ->
        praise "You have been duly diligent for this page."
      (_, True) ->
        damn "You still have something to do here."
      (False, False) -> mconcat
        [  worry s 
        ,  return " "
        ,  praise "You have done what you can, for now, "
        ,  damn "but you will need to return to this page."
        ]
-- special ("scoreboard", _) ds = (scoreboard ds <$> mo (Var "user"), [])
special ("vigenere",s) _ = case words s of
  [k] -> (, []) $ do
    let vigenere k c = chr (97 + mod (ord k + ord c - 97) 26)
    (v, h) <- textField ("vigenere" ++ filter isAlphaNum s) 8
    u <- mo (Var "user")
    c <- if zipWith vigenere (cycle k) u == v
      then do
        mo Pass
        praise "Thanks! Well done!"
      else do
        damn ("I haven't seen the code I'm looking for!")
    boxForm "vigenere" "Completion Code" (h ++ c)
  _ -> (return "", [])
special ("ascii", _) ds = (return (tag "pre" "" (B8.unpack (yfitned ds))), [])  -- ugh
special ("curriculum", _) cds =
  let (c, ds) = foldMap curriculum cds in (tag "ul" "" <$> c, ds)
special ("boxForm", nom) ds = case foldMap grok ds of
  (dm, dd) -> (boxForm (filter isAlphaNum nom) nom =<< dm, dd)
special ("cviq", s) ds = case words s of
  nom : caps ->  (boxForm nom (unwords caps) =<<
                 cviGrok (filter isAlphaNum nom) basicGs [] ds, [])
  _ -> (return ":cviq with no name", [])
special ("twitter", nom) _ = case words nom of
  (ac:_) -> (return $ concat
    [  "<a class=\"twitter-timeline\" href=\"https://twitter.com/", ac, "\">Tweets by ", ac, "</a>"
    ,  "<script async src=\"//platform.twitter.com/widgets.js\" charset=\"utf-8\"></script>"
    ], [])
  _ -> (return "", [])
special ("assoc", w) ds = case words w of
  (k : _) ->
    let (xms, dss) =
          unzip [((x, m), ds') | d <- ds, let (x, (m, ds')) = (id *** grok) (yankId d)]
    in  (, concat dss) $ do
          v <- mo $ Var k
          let ks = words v
          foldMap (\ (x, m) -> if elem x ks then m else return "") xms
  _ -> (return "", [])
special ("mcq", nom) ds =
  foldMap (mcqify (filter isAlphaNum nom)) (zip [1..] (map grok ds))
special ("huffman", s) _ = (huffProb (filter isUpper s), [])
special ("huffmark", s) _ = (huffSpesh (filter isUpper s), [])
special ("binRepPartA", _) _ = (binRepPartA, [])  -- ugh
special ("binRepPartB", _) _ = (binRepPartB, [])
special ("binRepPartC", _) _ = (binRepPartC, [])
special ("binRepPartD", _) _ = (binRepPartD, [])
special ("binArithPartA", _) _ = (binArithPartA, [])  -- ugh
special ("binArithPartB", _) _ = (binArithPartB, [])  -- ugh
special ("binArithPartC", _) _ = (binArithPartC, [])  -- ugh
special ("binArithPartD", _) _ = (binArithPartD, [])  -- ugh
special ("truthTableQ", s) ds = case (words s, foldMap grok ds) of
  ((n : xs), (dm, dd)) -> flip (,) dd $
    (boxForm n "" =<< ((++) <$> dm <*> ttq n (unwords xs)))
special ("negToVariableQ", s) ds = case (words s, foldMap grok ds) of
  ((n : xs), (dm, dd)) -> flip (,) dd $
    (boxForm n "" =<< ((++) <$> dm <*> evprob n (unwords xs)))
special ("disjunctiveNFQ", s) ds = case (words s, foldMap grok ds) of
  ((n : xs), (dm, dd)) -> flip (,) dd $
    (boxForm n "" =<< ((++) <$> dm <*> dnfq n (unwords xs)))
special ("tinyTrace", s) _ = case tinyTQString s of
  Nothing -> mempty
  Just (t, v, c) ->
    let nom = "tinyt" ++ filter isAlphaNum s
    in  (boxForm nom c =<< traceQ nom t v, [])
special ("traceQn", s) _ = case tinyTQString s of
  Nothing -> mempty
  Just (t, v, c) ->
    let (linesS:_) = splitOn '-' c
        nom = "tinyt" ++ filter isAlphaNum s
    in  (traceQTest nom t v (read linesS), [])
special ("traceQnAuto", s) _ = case tinyTQString s of
  Nothing -> mempty
  Just (t, v, c) ->
    let (linesS:_) = splitOn '-' c
        nom = "tinytraceAuto" ++ filter isAlphaNum s
    in  (traceAuto nom t v (read linesS), [])
special ("traceQnMark", s) ds = case tinyTQString s of
  Nothing -> mempty
  Just (t, v, c) ->
    let (linesS:_) = splitOn '-' c
        nom = "tinyt" ++ filter isAlphaNum s
        moo d = B8.unpack (yfitned [d])
    in  (traceQnMark nom t v (map moo ds), [])
special ("tinyAssembleGuess", s) ds = case tinyAQString s (B8.unpack (yfitned ds)) of
  Nothing -> mempty
  Just (p, v, c) ->
    let nom = "tinya" ++ filter isAlphaNum c
    in  (boxForm nom c =<< assQ nom AssGuess p v, [])
special ("tinyAssembleTrace", s) ds = case tinyAQString s (B8.unpack (yfitned ds)) of
  Nothing -> mempty
  Just (p, v, c) ->
    let nom = "tinya" ++ filter isAlphaNum c
    in  (boxForm nom c =<< assQ nom AssTrace p v, [])
special ("tinyAssembleOnly", s) ds = case tinyAQString s (B8.unpack (yfitned ds)) of
  Nothing -> mempty
  Just (p, v, c) ->
    let nom = "tinya" ++ filter isAlphaNum c
    in  (boxForm nom c =<< assQ nom AssOnly p v, [])
special ("tinyDisassemble", s) ds = case tinyAQString s (B8.unpack (yfitned ds)) of
  Nothing -> mempty
  Just (p, v, c) ->
    let nom = "tinyd" ++ filter isAlphaNum c
    in  (boxForm nom c =<< disQ nom p v, [])
special ("dscore", _) _ = flip (,) [] $ do
  (i, n) <- mo DScore
  return $ concat [show i, " points collected; ",
           show n, " deadlines expired"]
special ("tout", _) _ = flip (,) [] $ do
  mo Tout
special ("checkbox", s) ds = case words s of
  nom : _ ->
    let  blat (i, d) = do
           let x = "CHKBOX" ++ nom ++ i
           v <- mo $ Var x
           h <- fst $ grok d
           return $ concat
             [  "<input type=\"text\" "
             ,  "name=\"", x, "\" "
             ,  "value=\"0\" hidden"
             ,  ">"
             ,  "<input type=\"checkbox\" "
             ,  "name=\"", x, "\" "
             ,  "value=\"1\""
             ,  if v=="1" then " checked" else ""
             ,  ">"
             ,  h
             ]
    in   (foldMap (blat . yankId) ds, [])
special ("checkmark", s) ds = case words s of
  nom : _ ->
    let  blat (i, d) = do
           let x = "CHKBOX" ++ nom ++ i
           let x' = "CHKBOXMARK" ++ nom ++ i
           v <- mo $ Var x
           vd <- mo $ VarDef x
           v' <- mo $ Var x'
           v'd <- mo $ VarDef x'
           (e,f, d') <- case d of
             d@((i, s) :- ds) -> case dropWhile isSpace s of
               ('=':e) -> case exParse (pVExp "") e of
                 (_, Right e', _) -> (,,) "FMLA"
                    <$> eval boov [("not", not . any id), ("and", all id), ("or", any id)] e'
                    <*> pure ((i, "") :- ds)
                 _ -> return ("",Nothing, d)
               _ -> return ("",Nothing, d)
             _ ->  return ("",Nothing, d)
           let g = if v'd then v'=="1" else fromMaybe (v=="1") f
           q <- mo View
           h <- fst $ grok d'
           let m = case (q, vd, v'd, v == v') of
                 (Snoop _, _, _, _) -> concat
                   [  "<input type=\"text\" "
                   ,  "name=\"", x', "\" "
                   ,  "value=\"0\" hidden"
                   ,  ">"
                   ,  "<input type=\"checkbox\" "
                   ,  "name=\"", x', "\" "
                   ,  "value=\"1\""
                   ,  if g then " checked" else ""
                   ,  ">"
                   ,  e
                   ]
                 (_, True, False, _)    -> "<span style=\"color:green\">[unconfirmed]</span> &nbsp; "
                 (_, True, True, True)  -> "<span style=\"color:blue\">[confirmed]</span> &nbsp; "
                 (_, True, True, False) -> "<span style=\"color:red\">[denied]</span> &nbsp; "
                 _                      -> ""
           return $ concat
             [  "<input type=\"text\" "
             ,  "name=\"", x, "\" "
             ,  "value=\"0\" hidden"
             ,  ">"
             ,  "<input type=\"checkbox\" "
             ,  "name=\"", x, "\" "
             ,  "value=\"1\""
             ,  if v=="1" then " checked" else ""
             ,  ">"
             ,  m, h
             ]
    in   (foldMap (blat . yankId) ds, [])
  _ -> (return "", [])
special ("checkform", s) ds = case words s of   -- knocked off checkmark
  nom : _ ->
    let  blat (i, d) = do
           let x = "CHKBOXMARK" ++ nom ++ i
           v <- mo $ Var x
           vd <- mo $ VarDef x
           (e,f, d') <- case d of
             d@((i, s) :- ds) -> case dropWhile isSpace s of
               ('=':e) -> case exParse (pVExp "") e of
                 (_, Right e', _) -> (,,) "FMLA"
                    <$> eval boov [("not", not . any id), ("and", all id), ("or", any id)] e'
                    <*> pure ((i, "") :- ds)
                 _ -> return ("",Nothing, d)
               _ -> return ("",Nothing, d)
             _ ->  return ("",Nothing, d)
           h <- fst $ grok d'
           let m = case (vd, v, f) of
                     (False, _, _) -> "[you first]"
                     (True, _, Nothing) -> "[I'll trust you]"
                     (True, "0", Just False) -> "[YES]"
                     (True, "1", Just True) -> "[YES]"
                     (True, _, Just _) -> "[NO]"
           return $ concat
             [  "<input type=\"text\" "
             ,  "name=\"", x, "\" "
             ,  "value=\"0\" hidden"
             ,  ">"
             ,  "<input type=\"checkbox\" "
             ,  "name=\"", x, "\" "
             ,  "value=\"1\""
             ,  if v=="1" then " checked" else ""
             ,  ">"
             ,  m, h
             ]
    in   (foldMap (blat . yankId) ds, [])
  _ -> (return "", [])
special ("alert", _) _ = ("" <$ mo Alert, [])
special ("ifdef", s) ds = case words s of
  v : _ -> let (m, ds') = foldMap grok ds
           in  (,ds') $ do
                 x <- mo (VarDef v)
                 if x then m else return ""
  _ -> (return "", [])
special ("ifundef", s) ds = case words s of
  v : _ -> let (m, ds') = foldMap grok ds
           in  (,ds') $ do
                 x <- mo (VarDef v)
                 if not x then m else return ""
  _ -> (return "", [])
special ("tot", s) ds = case words s of
  sv : cm : _ -> (,[]) $ do
    let blat d = case exParse (pVExp ("CHKBOXMARK" ++ cm)) (B8.unpack (yfitned [d])) of
          (_, Right e, _) -> eval boov [("not", not . any id), ("and", all id), ("or", any id)] e
          _ -> return Nothing
    vs <- traverse blat ds
    case sequenceA vs of
      Just bs -> let s = show (fromIntegral (length (filter id bs)) / 2.0) in
        return $ concat
        ["<input type=\"text\" name=\"", sv, "\" "
        ,"value=\"", s, "\" hidden>", s
        ]
      Nothing ->  return "unknown"
  _ -> (return "", [])
special ("dump", _) _ = (mo Dump, [])
special ("staffonly", _) ds =
  let (m, ds') = foldMap grok ds
  in  (, ds') $ do
        mo View >>= \ x -> case x of
          Staff -> m
          _     -> return ""
special ("indent", _) ds = foldMap grok ds
special ("password", s) ds = case words s of
  nom : ps ->
    let ps = words s
        (m, ds') = foldMap grok ds
    in  (, ds') $ do
          (v, ph) <- textField nom 16
          if elem v ps
            then do
              x <- mo $ Var ("CONFIG" ++ v)
              if x == "1"
                then do
                  c <- m
                  return $ concat
                    ["<h3>Protected content available, thanks to the correct password here: "
                    ,ph,"</h3>"
                    ,c
                    ]
                else return $ concat
                    ["<h3>Protected content unavailable, despite the correct password here: "
                    ,ph,"</h3>"
                    ,"This password is currently deactivated."
                    ]                
            else return $ concat
              ["<h3>Protected content will be revealed by entering the correct password here: "
              ,ph,"</h3>"
              ]
  _ -> (return "", [])
special _ _ = mempty  -- for the now

{-
scoreboard :: [Dent] -> String -> String
scoreboard ds u = tag "table" "border=\"1\"" $ concat
  [ tag "tr" "" $ concat
    [ tag "th" "" "Topic"
    , tag "th" "" "Tests"
    , tag "th" "" "Best"
    ]
  , body
  , tag "tr" "" $ concat
    [ tag "th" "" "Total"
    , tag "td" "" ""
    , tag "td" "" (show total)
    ]
  ] where
  testScore u k t = getScores (k, t) u >>= \ (v, e) ->
   [( tag "a" (concat ["href=\"?node=test-", k, "-", t, "\""]) .
      tag "table" "border=\"1\"" . tag "tr" "" $ concat
        [ tag "td" "" t , tag "td" "" (show v), tag "td" "" e ]
    , v
    )]
  topic ((_, nom) :- ds) = case (words nom, words (yfitned ds)) of
    (k : _, ts) ->
      let ss = ts >>= testScore u k
          m = maximum (0 : map snd ss)
      in  ( tag "tr" "" $ concat
            [ tag "td" "" nom
            , tag "td" "" (ss >>= fst)
            , tag "td" "" (show m)
            ]
          , Sum m
          )
    _ -> ("", Sum 0)
  topic _ = ("", Sum 0)
  (body, Sum total) = foldMap topic ds
-}

cviGrok :: String -> [(String, Glob)] -> [(String, Glob)] -> [Dent] -> Mo String
cviGrok nom gs us [] = return ""
cviGrok nom gs us (e@((_, ':' : s) :- es) : ds) = case span isAlphaNum s of
  ("library", s) -> 
    let txt = es >>= (map B8.unpack . stned 0)
    in  ((if all isSpace s then "" else "<pre>" ++ unlines txt ++ "</pre>") ++) <$>
        case cviParse txt of
          Left (e, s) -> return ("Syntax error: wanted " ++ e ++
            if null s then "" else " but got <pre>" ++ s ++ "</pre>")
          Right rs -> case compile gs rs of
            Left b -> return ("Compilation error : " ++ show b)
            Right gs -> cviGrok nom gs us ds
  ("supply", s) -> cviGrok nom gs (filter ((`elem` words s) . fst) gs ++ us) ds
  ("editor", s) -> do
     let gs' = case words s of
           [] -> us ++ gs
           ks  -> us ++ filter ((`elem` ks) . fst) gs
     (src, hsrc) <- textArea (nom ++ "Editor" ++ show (length ds)) 30 60 ""
     let (fbsy, rs) = case cviParse (lines src) of
           Left (e, s) -> ("Syntax error: wanted " ++ e ++
             (if null s then "" else " but got <pre>" ++ s ++ "</pre>") ++ "<p/>", [])
           Right rs -> ("", rs)
     let (fbco, us') = case compile gs' rs of
           Left b -> ("Compilation error : " ++ show b ++ "<p/>", gs')
           Right us' ->
             (concat . reverse $ ["<code>" ++ f ++ " " ++ tyShow (ss :-> lts) ++ "</code><br/>"
                     | (f, (_, ss, lts)) <- us', isNothing (lookup f gs')]
             , us')
     (++) (hsrc ++ "</p>" ++ fbsy ++ fbco) <$> cviGrok nom gs us' ds
  ("tester", _) -> do
     (src, hsrc) <- textArea (nom ++ "Tester" ++ show (length ds)) 15 60 ""
     (concat [hsrc, "</p><pre>", unlines (getTests us src),"</pre><p/>"] ++) <$>
        cviGrok nom gs us ds
  ("assess", _) -> do
     let txt = B8.unpack (yfitned es)
     let fns = filter (isLower . head) (words txt)
     dts <- flip foldMap fns $ \ f -> case lookup f gs of
           Nothing -> return ("Assessment for undefined " ++ f ++ "!<br/>")
           Just (_, ss, lts) -> case lookup f us of
             Nothing -> damn ("You haven't defined <code>" ++ f ++ "</code> yet.<br/>")
             Just (_, ss', lts')
               | ss == ss' && lts == lts'
               -> praise ("Type ok: <code>" ++ f ++ " " ++ tyShow (ss :-> lts) ++ "</code><br/>")
               | otherwise
               -> damn ("Expected type: <code>" ++ f ++ " " ++ tyShow (ss :-> lts) ++ "</code><br/>" ++
                        "Delivered type: <code>" ++ f ++ " " ++ tyShow (ss' :-> lts') ++ "</code><br/>")
     cts <- compareTests (getTests gs txt) (getTests us txt)
     cts <- if null cts then praise "Everything is as it should be!" else return cts
     ((dts ++ cts) ++) <$> cviGrok nom gs us ds
  _ -> (++) <$> fst (grok e) <*> cviGrok nom gs us ds
cviGrok nom gs us (d : ds) = (++) <$> fst (grok d) <*> cviGrok nom gs us ds

compareTests :: [String] -> [String] -> Mo String
compareTests [] _ = return ""
compareTests (s : ss) (u : us)
  | s == u = compareTests ss us
  | u == "Undefined: " ++ takeWhile isAlphaNum s = compareTests ss (u : us)
  | isPrefixOf "Undefined: " u = (++) <$> damn (u ++ "<br/>") <*> compareTests (s : ss) us
  | fst (span (/= '-') s) == fst (span (/= '-') u)
  = (++)  <$> damn (concat ["Expected: <code>", s, "</code><br/>Delivered: <code>", u, "</code><br/>"])
           <*> compareTests ss us
  | otherwise = let f = takeWhile isAlphaNum s
                in  (++) <$> damn ("Not testing <code>" ++ f ++
                               "</code> until the type is right.<br/>")
                     <*>  compareTests (dropWhile (isPrefixOf f) ss)
                            (dropWhile (isPrefixOf f) (u : us) )
compareTests _ [] = damn "Something's missing!<br/>"


mcqify :: String -> (Int, (Mo String, [Dent])) -> (Mo String, [Dent])
mcqify k (v, (m, ds)) = flip (,) ds $ do
  u <- mo $ Var k
  s <- m
  let (b, s') = case s of {'!' : s -> (True, s) ; s -> (False, s)}
  let c = u == show v
  msg <- case (b, c) of
    (True, True) -> praise " Yes!"
    (False, True) -> damn " No!"
    (True, False) -> damn ""
    (False, False) -> return ""
  return $ tag "input"
        (concat ["type=\"radio\" name=", show k, " value=", show v,
                 if c then " checked" else ""])
        (s' ++ msg ++ "<br/>")


curriculum :: Dent -> (Mo String, [Dent])
curriculum d@(_ :- ds) = case words (dHead d) of
  [] -> (return "", [])
  n : ns -> case foldMap grok ds of
   (p, ds) -> flip (,) ds $ do
    np <- mo (Passed n)
    nps <- all id <$> traverse (mo . Passed) ns
    l <- case (np, nps) of
      (True, _)       -> praise "[OWNED] "
      (False, True)   -> mconcat [damn "", worry "[ACTIVE] "]
      (False, False)  -> damn "[DANGER] "
    s <- p
    sn <- (</> n) <$> mo (Var "node")
    return $ tag "li" "" $ tag "a" ("href=\"?page=" ++ sn ++ "\"") (l ++ s)


grokBox :: Mo String
grokBox = do
  mo (VarChange "grokComment") >>= \ x -> if x then
    mo Alert else return ()
  boxForm "grokBox" "Pause for Thought" =<< mconcat
    [  radioButtons "grok"
         [  ("ok", "This page makes sense to me.", "")
         ,  ("hmm", "I've read this page, but I don't fully get it.", "")
         ,  ("err", "I've read this page, and I feel completely lost.", "")
         ,  ("doh", "I haven't read this page.", "")
         ]  (not . (<= "doh"))
    ,  return "Comment<br/>"
    ,  snd <$> textArea "grokComment" 5 80 ""
    ,  return "<br/>"
    ,  mo View >>= \x -> case x of
         Snoop _ -> snd <$> textArea "grokResponse" 5 80 ""
         _       -> mo (Var "grokResponse")
    ,  return "<br/>"
    ]

forthcoming :: Mo String
forthcoming = do
    es <- mo EventListings
    us <- mo EventUpdates
    vt <- mo VTime
    gs <- mo Groups
    return $ tag "div" "id=attendance" $ concat
      [  tag "h2" "" "Forthcoming Attractions"
      ,  foldMap evAd (reverse [e | e <- es,
           vt < evTime e,
           null (evGroups e) || not (null (intersect (evGroups e) gs))])
      ]
  where
    evAd e = tag "p" "" . tag "fieldset" "" $ concat
      [  tag "legend" "" $ unwords [evKind e, evNumber e, evTitle e]
      ,  evTimeText e, if null (evOwner e) then "" else " with " ++ evOwner e
      ,  " in ", evPlace e
      ]

attendance :: Mo String
attendance = do
    es <- mo EventListings
    us <- mo EventUpdates
    vt <- mo VTime
    gs <- mo Groups
    return $ tag "div" "id=attendance" $ concat
      [  tag "h2" "" "Were you there?"
      ,  foldMap evForm [e | e <- es,
           evTime e <= vt,
           null (evGroups e) || not (null (intersect (evGroups e) gs)),
           notYet (euk e) us]
      ]
  where
    euk e = "event" ++ evKind e ++ evNumber e
    notYet k us = null [updateValue u | u <- us, updateKey u == k]
    evForm e = tag "p" "" . tag "form" "method=\"post\"" . tag "fieldset" "" $ concat
      [  tag "legend" "" $ unwords [evKind e, evNumber e, evTitle e]
      ,  evTimeText e, if null (evOwner e) then "" else " with " ++ evOwner e
      ,  " in ", evPlace e
      ,  "<br/>"
      ,  tag "input" ("type=\"radio\" value=\"Yes\" name=\"" ++ euk e ++ "\"")
           "Yes, I was there. &nbsp; "
      ,  tag "input" ("type=\"radio\" value=\"No\" name=\"" ++ euk e ++ "\"")
           "No, I missed it. &nbsp; "
      ,  tag "input" ("type=\"radio\" value=\"Phew\" name=\"" ++ euk e ++ "\"")
           "Phew, I missed it, but I've caught up."
      ,  "<br/>"
      ,  "Comment: "
      ,  tag "textarea" ("name=\"" ++ euk e ++ "Comment\" rows=\"5\" cols=\"80\"")
           ""
      ,  tag "input" ("type=\"submit\" value=\"Send\"") ""
      ]


editAttendance :: Mo String
editAttendance = do
    es <- mo EventListings
    gs <- mo Groups
    us <- nubBy (\ u v -> updateKey u == updateKey v) <$> mo EventUpdates
    let es' = [e | u <- us, e <- es, updateKey u == euk e]
    vt <- mo VTime
    return $ tag "div" "id=attendance" $ concat
      [  tag "h2" "" "Were you there?"
      ,  foldMap (evForm us)
          [e | u <- us, e <- es', updateKey u == euk e,
               null (evGroups e) || not (null (intersect (evGroups e) gs))]
      ]
  where
    euk e = "event" ++ evKind e ++ evNumber e
    evForm us e =
     tag "p" "" . tag "form" "method=\"post\"" . tag "fieldset" "" $ concat
      [  tag "legend" "" $ unwords [evKind e, evNumber e, evTitle e]
      ,  evTimeText e, if null (evOwner e) then "" else " with " ++ evOwner e
      ,  " in ", evPlace e
      ,  "<br/>"
      ,  tag "input" ("type=\"radio\"" ++ check "Yes" e us ++
           "name=\"" ++ euk e ++ "\"")
           "Yes, I was there. &nbsp; "
      ,  tag "input" ("type=\"radio\"" ++ check "No" e us ++
           "name=\"" ++ euk e ++ "\"")
           "No, I missed it. &nbsp; "
      ,  tag "input" ("type=\"radio\"" ++ check "Phew" e us ++
           "name=\"" ++ euk e ++ "\"")
           "Phew, I missed it, but I've caught up."
      ,  "<br/>"
      ,  "Comment: "
      ,  tag "textarea"
           ("name=\"" ++ euk e ++ "Comment\" rows=\"5\" cols=\"80\"")
           (getComment e us)
      ,  tag "input" ("type=\"submit\" value=\"Send\"") ""
      ]
    getComment e us =
      case [updateValue u | u <- us, updateKey u == euk e ++ "Comment"] of
        (v : _) -> v
        _ -> ""
    check v e us =
      case [updateValue u | u <- us, updateKey u == euk e] of
        (v' : _) | v == v' -> " value=" ++ show v ++ " checked "
        _ -> " value=" ++ show v ++ " "
