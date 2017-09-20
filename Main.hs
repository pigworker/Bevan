{-# LANGUAGE OverloadedStrings, GADTs, TupleSections #-}

module Main where

import System.Process
import System.Environment
import System.Directory
import Data.ByteString.Lazy as B (ByteString, concat, getContents)
import Data.ByteString.Lazy.Char8 as B8 (putStrLn, pack, unpack, readFile)
import Data.PHPSession
import Data.List as L
import System.FilePath
import System.IO.Error
import System.Random
import Data.Char
import Data.Maybe
import Storage
import Dent
import Events
import Sig
import Grok
import QuizUtils

urlRoot :: String
urlRoot = "https://personal.cis.strath.ac.uk/conor.mcbride/Marx/?page="

pageRoot :: FilePath
pageRoot = "/home/s3/cgb08121/WWW/MarxPages/"

logRoot :: FilePath
logRoot = "/home/s3/cgb08121/WWW/MarxLogs/"

tessaRoot :: FilePath
tessaRoot = "/home/s3/cgb08121/WWW/POSTESSA/data/"

run1 :: Sig r -> ENV -> STA -> (r, STA)
run1 (Var k) e s = case lookup k (vars e) of
  Nothing  -> ("", s)
  Just v   -> (v, s)
run1 (VarDef k) e s = case lookup k (vars e) of
  Nothing  -> (False, s)
  Just v   -> (True, s)
run1 (VarChange k) e s = (diffy k (vars e), s) where
  diffy k [] = False
  diffy k ((k', v) : kvs) | k == k' = case lookup k kvs of
    Just v' -> v /= v'
    Nothing -> not (null v)
  diffy k (_ : kvs) = diffy k kvs
run1 PTitle e s = (pTitle e, s)
run1 VTime e s = (vTime e, s)
run1 EventListings e s = (eventListings e, s)
run1 EventUpdates e s = (eventUpdates e, s)
run1 Damn e s = ((), s {damnation = True})
run1 Damned e s = (damnation s, s)
run1 (Score i) e s = ((), s {outcome = show i})
run1 Pass e s = ((), s {outcome = "PASS"})
run1 (Passed n) e s =
  let np = fromMaybe "" (lookup "node" (vars e))
  in  (any (\ u -> updateKey u == (np </> n)) (scores e), s)
run1 (Rnd ran) e s = (r, s {seed = s'}) where (r, s') = randomR ran (seed s)
run1 DScore e s = (dscore e, s)
run1 Tout e s = (tout e, s)
run1 View e s = (view e, s)
run1 Alert e s = ((), s {alert = True})
run1 Dump e s = (show (vars e), s)

runMo :: Mo x -> ENV -> STA -> (x, STA)
runMo (Ret x) e s = (x, s)
runMo (c :? g) e s =
  let (r, s') = run1 c e s in runMo (g Ret r) e s'

globals :: String -> [(String,String)]
globals s = [g | x <- lines s, Just g <- [glom x]] where
  glom x = case span (not . isSpace) x of
    (k@(_:_), v) -> Just (k, dropWhile isSpace v)
    _ -> Nothing

makePage ::  [String]{-node-} -> String{-time-} ->
             Mo String{-pagemaker-} -> [Dent]{-stuff-} ->
             -- (String, String, String, View) {-username, realname, 3 words, view-} ->
             [(String, String)] {-inputs-} ->
             IO (String{-html-} {-,
                  (STA{-outputs-}, PageInfo{-quiz-}, PageInfo{-scores-}, PageInfo{-events-})-})
makePage [] _ _ _ {-_-} _ =
  return ("No class: no content!" {-, (nulSTA, PageInfo [][], PageInfo [][], PageInfo [][])-})
makePage np@(cla : _) vtime page stuff {-(u, n, tlws, v)-} inputs = do
  let ranseed = mkStdGen (sum ({- map ord u ++ -} map ord (joinPath np)))
  -- qi <- dopeOn logRoot u np                                 -- no user-specific data
  -- gs <- globals <$> tryRead 3 (logRoot </> u </> "GLOBAL")
  -- ei <- dopeOn logRoot u ["hide-events"]
  -- sci <- dopeOn logRoot u ["SCORES"]
  conf <- dopeOn pageRoot "CONFIG" np
  evs <- (foldMap evDent . dentify . B8.pack {-ugh!-})
            <$> tryRead 3 (pageRoot </> cla </> "EVENTS")
  -- vs <- getVars
  let (eis, isAll) = partition (isPrefixOf "event" . fst) inputs
  let (cis, is) = partition (isPrefixOf "CONFIG" . fst) inputs
  let eu = [PageUpdate k vtime v | (k, v) <- eis] {- ++ pageUpdates ei -}
  -- let os = [(updateKey r, updateValue r) | r <- pageUpdates qi]
  let cs = [("CONFIG" ++ updateKey r, updateValue r) | r <- pageUpdates conf]
  let env =  ENV
             {  vars = 
                  [{-("user", u),-} ("name", "Guest"), ("class", cla), ("node", joinPath np),("threewords","who are you")]
                  ++ isAll ++ {- vs ++ os ++ -} cs {- ++ gs -}
             ,  pTitle = head [s | Just s <- map getTitle stuff ++ [Just "No Title Found"]]
             ,  eventListings = evs
             ,  eventUpdates = eu
             ,  vTime = vtime
             ,  scores = [] -- pageUpdates sci
             ,  dscore = (0,0) -- scoreSoFar vtime sci
             ,  tout = "" -- testOut
             ,  view = Student
             }
  let ista = STA
             {  upState =
                  {-[PageUpdate k vtime v | (k, v) <- is, lookup k os /= Just v]
                  ++ -} [PageUpdate "VISIT" vtime ""] {- ++ pageUpdates qi -}
             ,  reConf = conf {pageUpdates =
                  [PageUpdate k vtime v | (x@('C':'O':'N':'F':'I':'G':k), v) <- cis, lookup x cs /= Just v]
                  ++ [PageUpdate "VISIT" vtime ""] ++ pageUpdates conf}
             ,  damnation = False
             ,  outcome = ""
             ,  seed = ranseed
             ,  alert = False
             }
  let (h, osta) = runMo (page >>= wrapPage) env ista
  return (h {-, (osta, qi, sci, ei {pageUpdates = eu})-})

data ENV = ENV
  {  vars :: [(String, String)]
  ,  pTitle :: String
  ,  eventListings :: [Event]
  ,  eventUpdates :: [PageUpdate]
  ,  scores :: [PageUpdate]
  ,  vTime :: String
  ,  dscore :: (Int, Int)
  ,  tout :: String
  ,  view :: View
  }
data STA = STA
  {  upState :: [PageUpdate]
  ,  reConf :: PageInfo
  ,  damnation :: Bool
  ,  outcome :: String
  ,  seed :: StdGen
  ,  alert :: Bool
  }
nulSTA :: STA
nulSTA = STA [] (PageInfo [] []) True "" (mkStdGen 0) False


getTitle :: Dent -> Maybe String
getTitle ((_, '#':'t':'i':'t':'l':'e':' ':s) :- _) = case span isSpace s of
  (_, t) -> Just t
getTitle _ = Nothing

wrapPage :: String -> Mo String
wrapPage p = do
  pt <- mo PTitle
  nom <- mo $ Var "name"
  return . tag "html" "" $ L.concat
    [  tag "head" "" $ L.concat
         [  tag "title" "" pt
         ,  tag "link" (attrsL
            [  ("rel","stylesheet")
            ,  ("type","text/css")
            ,  ("href","marx.css")
            ]) ""
         ]
    ,  tag "body" "" $ L.concat
       [  tag "h1" "" pt
       ,  p
       ]
    ]

shitPost :: PHPSessionValue -> [(String, String)]
shitPost (PHPSessionValueArray kvs) =
  [ (B8.unpack k, B8.unpack v)
  | (PHPSessionValueString k, PHPSessionValueString v) <- kvs
  ]
shitPost _ = []

fop :: [(String,String)] -> [(String,String)]
fop ((k, v) : (k', _) : kvs) | k == k' = (k, v) : kvs
fop (x : xs) = x : fop xs
fop [] = []

main :: IO ()
main = do
  vtime <- timeStamp
  xs <- getArgs
  pbs <- B.getContents
  let mpost = L.unfoldr decodePartialPHPSessionValue pbs
  let (inputs, getty) = case mpost of
        (post : gett : _) -> (fop (shitPost post), shitPost gett)
        _ -> ([], [])
  case xs of
    (u : p : _) -> do
      classDirs <- listDirectory (tessaRoot </> "classes")
      let classTable =
            [  (cc, (co, cl))
            |  d <- classDirs
            ,  (cl, ('-':co:'-':cc)) <- [span (/= '-') d]
            ]
      let np = map (filter (/= '/')) (splitPath p)
      (n, tlws, u, v) <- case np of
        (c : _) -> case lookup c classTable of
          Just (co, _) -> do
            xs <- catchIOError (splitOn ':' <$>
                    readProcess "grep" [u, tessaRoot </> "cohorts" </> [co]] "")
                    (\ _ -> return [])
            case xs of
              (_:_:tlws:_:s:f:e:k:_) ->
                if k == "staff"
                  then case lookup "snoop" getty of
                    Nothing -> return (L.concat [f, " ", s], tlws, u, Staff)
                    Just u  -> do
                      xs <- catchIOError (splitOn ':' <$>
                             readProcess "grep" [u, tessaRoot </> "cohorts" </> [co]] "")
                             (\ _ -> return [])
                      case xs of
                        (_:_:tlws:_:s':f':_) ->
                          return (L.concat [f, " ", s, " snooping on ",f', " ",s'],
                                  tlws, u, Snoop u)
                  else return (L.concat [f, " ", s], tlws, u, Student)
              _ -> return ("A.N. Other", "??? ??? ???", u, Student)
          _ -> return ("A.N. Other", "??? ??? ???", u, Student)
        _ -> return ("A.N. Other", "??? ??? ???", u, Student)
      -- B8.putStrLn (B8.pack (n ++ " " ++ tlws))
      (rlz, mrx) <- catchIOError
        ((True,) <$> (B8.readFile (pageRoot </> p </> "node.mrx"))) $ \
        e -> return (False,
                B.concat ["Can't find ", B8.pack p, " ",
                      B8.pack (show e) ])
      -- B8.putStrLn mrx
      {--}                      
      let (page, stuff) = foldMap grok (dentify mrx)
      (h {-, (osta, qi, sci, evi)-})
        <- makePage np vtime page stuff {-(u, n, tlws, v)-} inputs
      {-
      let (_, qu) = partition (isPrefixOf "event" . updateKey) (upState osta)
      let qo = qi {pageUpdates = qu}
      if rlz then do
          case (damnation osta,
                if null (outcome osta) then "PASS" else (outcome osta),
                [updateValue u | u <- pageUpdates sci, updateKey u == p]) of
            (True, _, _) -> return ()
            (False, s, s' : _) | s <= s' -> return () -- score unimproved
            (False, s, _) -> dopeUp logRoot u ["SCORES"]
               (sci {pageUpdates =
                PageUpdate (if null np then "." else joinPath np) vtime s
                : pageUpdates sci})
          dopeUp logRoot u np qo
        else return ()
      case v of
        Staff -> dopeUp pageRoot "CONFIG" np (reConf osta)
        _ -> return ()
      if alert osta
        then appendFile (logRoot </> "ALERTS") $ L.concat
          [n, "!", vtime, "!",p ,"!", urlRoot, p, "&snoop=", u, "\n"
          ]
        else return ()
      -}
      B8.putStrLn (B8.pack h)
      {--}
    _ -> B8.putStrLn $ B.concat
           [  "<html><body>Either I don't know who you are "
           ,  "or I don't know what to say.</body></html>"
           ]
