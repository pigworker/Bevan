> module TINYQuizKit where

> import Control.Monad
> import Control.Monad.State
> import Control.Monad.Except
> import Control.Monad.Trans.Except
> import Control.Applicative
> import Data.Traversable
> import Data.Foldable hiding (elem, all, foldl, concat, sum)
> import Data.Maybe
> import Data.Monoid
> import Data.Char
> import Data.List
> import Text.XHtml
> import Numeric

> import Sig
> import TINY hiding (header1, header2, blank)
> import TINYAss
> import QuizUtils
> import HackOldQuiz
> import ExParse

>   -- TODO: why are there two trailing spaces?
>   -- (copied from traceQ)
>   -- There is a copy without spaces in TINY.hs
> header1 = "I L F A  Memory----------  Action---  "
> header2 = "P I R C  0123456789ABCDEF  OPR & ? !  "
> blank = ". . . .  ................  ### - - -  "

> grrr :: String -> Html
> grrr (' ' : ' ' : s) = primHtml "&nbsp;&nbsp;" +++ grrr s
> grrr (c : s) = (id << c) +++ grrr s
> grrr [] = noHtml


> traceQTest :: String -> TINY -> [V] -> Int -> Mo String
> traceQTest k ti is rows = do
>   let iq = (id << "Input queue: " +++
>           if null is then italics << "empty" else
>             foldl (+++) noHtml
>               (intersperse (id << ", ") (map ((id <<) . shex) is)))
>   let sti = shtiny ti
>   let textHeader = "The starting configuration is:"
>   let koh = grrr sti +++ primHtml (concat (replicate 13 "&nbsp;"))
>   (s, ah) <- textArea (k ++ "trace") (rows + 3) 38 (unlines $ [header1, header2, sti ++ "  ### - - -"] ++ replicate rows blank)
>   let ls = lines s
>   doesItParse <- parset ti ls
>   frs <- primHtml <$> radioButtons (k ++ "finish")
>     [  ("Halted", "halted normally", "")
>     ,  ("Looping", "loops henceforth", "")
>     ,  ("Starved", "starved for input", "")
>     ]  (not . null)
>   let fh = paragraph <<
>              [  id << "In the end, the process", br , frs
>              ]
>   return . renderHtmlFragment <<
>     [  iq, br
>     ,  table <<
>        [  tr << [td << textHeader, td noHtml]
>        ,  tr << [td ! [align "center"] << thecode (grrr header1), td noHtml]
>        ,  tr << [td ! [align "center"] << thecode (grrr header2), td noHtml]
>        ,  tr << [td ! [align "center"] << thecode koh, td noHtml]
>        ,  tr << [td ! [align "center", valign "top"] << primHtml ah
>                 ,td ! [valign "top"] << doesItParse]
>        ]
>     , fh
>     ]

> -- given a starting configuration, give feedback as to whether the user's
> -- input is:
> --   optional tiny header
> --   starting configuration  <action>
> -- then multiple rows of
> --   <config>  <action>
> -- where <action> is: <OpCode> <HexOrHyphen> <HexOrHyphen> <HexOrHyphen>
> -- and <config> is: (<HexOrDot> ){4} <HexOrDot>{16}
> -- TODO: there are choices on feedback:
> --       we want to check it parses, as that will reduce manual marking burden
> --       but what about: check correct starting position
> --                     : hex digit, or just alpha-numeric
> --                     : is something an actual op code
> -- TODO: what is the format for starving for input -- the parser may not like it!
> parset :: TINY -> [String] -> Mo Html
> --parset ti ss = (concatHtml . intersperse br . map thecode) <$> (adviseHead $ parseTrace ss)
> parset ti ss = (concatHtml . intersperse br . map thecode) <$> (adviseHead $ parseTrace ss)
>   where adviseHead (Head1:Head2:ls) = (:) <$> qpraise ("head1 Ok"+++ br +++ "head2 Ok") <*> advise ls
>         adviseHead ls = advise1 ls
>
>         -- advise1 : check they copied the starting config correctly
>         advise1 ((TinyOp t op):ls) = (:) <$> checkCopy t
>                                          <*> advise ls
>         advise1 ((TinyBlankOp t):ls) = (:) <$> checkCopy t
>                                            <*> advise ls
>         advise1 ((TinyNoOp t):ls) = (:) <$> checkCopy t
>                                         <*> advise ls
>         advise1 ls = advise ls
>
>         advise [] = return []
>         advise (Head1:ls) = (:) <$> abuseHead <*> advise ls
>         advise (Head2:ls) = (:) <$> abuseHead <*> advise ls
>         advise (Blank:ls) = (:) <$> (qpraise << "blank line ok")
>                                 <*> advise ls
>         advise ((TinyOp t op):ls) = (:) <$> checkValid t "TinyOp" <*> advise ls
>         advise ((TinyBlankOp t):ls) = (:) <$> checkValid t "TinyBlankOp" <*> advise ls
>         advise ((TinyNoOp t):ls) = (:) <$> checkValid t "TinyNoOp" <*> adviseNoOp ls
>         advise ((Bad parsed Clueless rest):ls) = (:) <$> abuse << ("Couldn't understand " ++ parsed ++ rest ++ ".")
>                                            <*> advise ls
>         advise ((Bad _ (Thing err _) rest):ls) = (:) <$> abuse << ("Expected " ++ err ++ " and got '" ++ rest ++ "'.")
>                                            <*> advise ls
>         adviseNoOp [] = return []
>         adviseNoOp _ = (:[]) <$> abuse << "You either need to stop the trace (delete all lines below here), or give an OpCode above"
>
>         checkCopy t = if t==ti
>                       then qpraise << "Copied start config correctly"
>                       else abuse << "This doesn't look like starting config to me"
>
>         abuseHead = abuse << "Can only have a header at the beginning"
>
>         checkValid t s = if isValid t
>                          then qpraise << (s++" ok")
>                          else abuse << "you need to fully specify the machine state before using blanks '.'"

> traceAuto :: String -> TINY -> [V] -> Int -> Mo String
> traceAuto k ti is rows = do
>   let iq = (id << "Input queue: " +++
>           if null is then italics << "empty" else
>             foldl (+++) noHtml
>               (intersperse (id << ", ") (map ((id <<) . shex) is)))
>   let ((o, ths), to) = tiny ti is
>   let sti = shtiny ti
>   let textHeader = "The starting configuration is:"
>   let koh = grrr sti +++ primHtml (concat (replicate 13 "&nbsp;"))
>   (s, ah) <- textArea (k ++ "traceAuto") (rows + 3) 38 $ unlines [header1, header2, showTrace ths to]
>   let fh = paragraph <<
>              [  primHtml $ "In the end, the process " ++ showOutcome o
>              ]
>   return . renderHtmlFragment <<
>     [  iq, br
>     ,  table <<
>        [  tr << [td << textHeader, td noHtml]
>        ,  tr << [td ! [align "center"] << thecode (grrr header1), td noHtml]
>        ,  tr << [td ! [align "center"] << thecode (grrr header2), td noHtml]
>        ,  tr << [td ! [align "center"] << thecode koh, td noHtml]
>        ,  tr << [td ! [align "center", valign "top"] << primHtml ah ! [strAttr "readonly" "readonly"]]
>        ]
>     , fh
>     ]

> data TTr
>   = TInit TINY
>   | TStep TrOut (TINY, [V])
>   | TStop Outcome
>   deriving Show

> nachster :: TINY -> [TraceLine] -> TINY
> nachster _ (TinyOp t _ : _) = t
> nachster _ (TinyBlankOp t : _) = t
> nachster _ (TinyNoOp t : _) = t
> nachster t (_ : ls) = nachster t ls
> nachster t [] = t

> traceTTr :: [Bool] -> (TINY, [V]) -> [TraceLine] -> [([Bool], (TINY, [V]), TTr)]
> traceTTr bs s@(t, is) ls = (bs, s, TInit t') : steps (heat bs t t') (t', is) ls where
>   heat bs t t' = zipWith3 hot bs (mem t) (mem t')
>   hot b v v' = b || (v /= v')
>   t' = nachster t ls
>   steps bs s@(t, is) (TinyOp _ a@(_,_,i,_) : ls)
>     = (bs, s, TStep a s') : steps (heat bs t t') s' ls
>     where
>       t' = nachster t ls
>       s' = (t', drop (case i of {Nothing -> 0; _ -> 1}) is)
>   steps bs s (_ : ls) = steps bs s ls
>   steps bs s [] = [(bs, s, TStop Halted)] -- really?

> markDecode :: ([Bool], (TINY, [V]), TTr) ->
>   (  Bool   -- decoded correctly?
>   ,  Bool   -- involved hot memory?
>   )
> markDecode (hs, (t, is), TInit t') = (t == t', False)
> markDecode (hs, (TINY ip _ _ ac vs, is), TStep action _)
>   = (action == (o, a, g, p), h)
>   where
>     ip' = m16 ip
>     o = toEnum (vs !! ip')
>     ip1 = m16 (ip' + 1)
>     (a, h) = if elem o [JMP, JZE, JNZ, LDA, STA, ADC, LDL]
>       then (Just (vs !! ip1), (hs !! ip') || (hs !! ip1))
>       else (Nothing, hs !! ip')
>     g = case (o, is) of
>           (GET, i : _)  -> Just i
>           _             -> Nothing
>     p = case o of
>           PUT -> Just ac
>           _   -> Nothing
> markDecode (hs, (TINY _ _ fr _ _, _), TStop _) = (fr > 7, False)  -- need to fix for starving

> markUpdate :: ([Bool], (TINY, [V]), TTr) -> Bool
> markUpdate (_, s, TStep action s') = case runExcept (runStateT step s) of
>   Right (a'@(o,_,_,_), s'') -> (action == a') && ((s' == s'') || (o == HLT))
>   Left _ -> False
> markUpdate (_, _, TInit _) = True
> markUpdate (_, (TINY _ _ fr _ _, _), TStop _) = fr > 7

> critOn :: [(([Bool], (TINY, [V]), TTr), (Bool, Bool), Bool)] -> String -> (Bool, String)
> critOn td cr = case span (/= '=') cr of
>     (c, _ : x) -> (check (words c), x)
>     _ -> (False, "")
>   where
>     check ("decode" : n : cs) = length [() | (_, (True, _), _) <- td] >= read n && check cs
>     check ("selmods" : cs) =
>       (length [() | (_, (True, True), _) <- td] >= 1) &&
>       null [() | (_, (False, True), _) <- td] &&
>       check cs
>     check ("selmod" : n : cs) = length [() | (_, (True, True), _) <- td] >= read n && check cs
>     check ("correct" : cs) = all id (concat [[a,c] | (_, (a, _), c) <- td]) && check cs
>     check (o'@[_,_,_] : n : cs) =
>         length [() | ((_, _, TStep (o,_,_,_) _), (True, _), True) <- td, o == read o'] >= read n
>         && check cs
>     check [] = True
>     check  _ = False

> traceQnMark :: String -> TINY -> [V] -> [String] -> Mo String
> traceQnMark k ti is crs = do
>   ls <- lines <$> (mo $ Var (k ++ "trace"))
>   let trs = traceTTr (replicate 16 False) (ti, is) (parseTrace ls)
>   let td = [(tr, markDecode tr, markUpdate tr) | tr <- trs]
>   let mks = map (critOn td) crs
>   let row (b, x) = concat
>        [  "<tr><td>", x, "</td><td>"
>        ,  if b then "yes" else "no"
>        ,  "</td></tr>"
>        ]
>   let score = show (fromIntegral (length (filter fst mks)) / 2.0)
>   return $ concat
>     ["This table shows what we were looking for. Each is worth half a mark.<br/>"
>     ,"<table border=\"1\">"
>     ,mks >>= row
>     ,"</table><br/>"
>     ,"<input type=\"text\" name=\"testScore\""
>     ,"value=\"", score, "\" hidden>"
>     ,"<b>Press \"Send\" to confirm your score!</b>"
>     ]

> traceQ :: String -> TINY -> [V] -> Mo String
> traceQ k ti is = do
>   let iq = (id << "Input queue: " +++
>           if null is then italics << "empty" else
>             foldl (+++) noHtml
>               (intersperse (id << ", ") (map ((id <<) . shex) is)))
>   let ((o, ths), to) = tiny ti is
>   let sti = shtiny ti
>   (s, ah) <- textArea (k ++ "trace") (length ths + 2) 38 (sti ++ "  ")
>   let cap1 = thecode <<
>        primHtml "I L F A&nbsp;&nbsp;Memory----------&nbsp;&nbsp;Action---&nbsp;&nbsp;"
>   let cap2 = thecode <<
>        primHtml "P I R C&nbsp;&nbsp;0123456789ABCDEF&nbsp;&nbsp;OPR & ? !&nbsp;&nbsp;"
>   let koh = thecode << (grrr sti +++
>                         primHtml (concat (replicate 13 "&nbsp;")))
>   let ls = lines s
>   mh <- markt ti ls ths to
>   frs <- primHtml <$> radioButtons (k ++ "finish")
>     [  ("Halted", "halted normally", "")
>     ,  ("Looping", "loops henceforth", "")
>     ,  ("Starved", "starved for input", "")
>     ]  (not . null)
>   fv <- mo $ Var (k ++ "finish")
>   fm <- if show o == fv
>           then qpraise (id << "That's right!") else abuse (id << "Try again!")
>   let fh = paragraph <<
>              [  id << "In the end, the process", br , frs +++ fm
>              ]
>   return . renderHtmlFragment <<
>     [  iq, br
>     ,  table <<
>        [  tr << [td ! [align "center"] << cap1, td noHtml]
>        ,  tr << [td ! [align "center"] << cap2, td noHtml]
>        ,  tr << [td ! [align "center"] << koh, td noHtml]
>        ,  tr << [td ! [align "center", valign "top"] << primHtml ah,
>                  td ! [valign "top"] << mh]
>        ]
>     ,  fh
>     ]

> markt :: TINY -> [String] -> [(TINY, TrOut)] -> TINY -> Mo Html
> markt _ [] [] to = abuse << "What configuration have we ended up in?"
> markt ti (s : ss) [] to = case markConf ti s to of
>   ("", _) -> qpraise << [thecode << s, br, id << "I've seen enough!"]
>   (a, _) -> abuse << [thecode << s, br, id << a, br]
> markt _ [] _ _ = abuse << "Now, what happens next?"
> markt ti (s : ss) (h@(tn, _) : hs) to = case markLine (null ss) ti s h of
>   "" -> (+++) <$> (qpraise << [thecode << s, br]) <*> markt tn ss hs to
>   a -> abuse << [thecode << s, br, id << a, br]

> tinyTQString :: String -> Maybe (TINY, [V], String)
> tinyTQString s =
>   case exParse ((,,) <$ spc  <*> tinyP dullTINY <* spc
>                             <*> many hexP <* spc 
>         <*> ("" <$ eoi <|> id <$ toks "-" <*> many (eat (const True)) <* eoi)) s of
>     (_, Right tvs, _) -> Just tvs
>     _ -> Nothing

> tinyAQString :: String -> String -> Maybe (Program, [V], String)
> tinyAQString s p =
>   case exParse ((,) <$ spc <*> many hexP <* spc 
>         <*> ("" <$ eoi <|> id <$ toks "-" <*> many (eat (const True)) <* eoi)) s of
>     (_, Right (v, c), _) -> case exParse program p of
>        (_, Right cs, _) -> Just (cs, v, c)
>        _ -> Nothing
>     _ -> Nothing


> qhex :: String -> Maybe V -> Mo (Maybe V, String, String)
> qhex k v = do
>   (g, h) <- textField k 1
>   case (v, all isSpace g, exParse (spc *> hex <* spc) g) of
>     (Just i, False, (_, Right j, _)) -> 
>       if i == j then (,,) (Just i) h <$> (praise "OK")
>                 else (,,) (Just j) h <$> (damn "No!")
>     (Just i, False, _) -> (,,) Nothing h <$> (damn "Hex?")
>     (Just _, True, _) -> (,,) Nothing h <$> (damn "" >> worry "Ready")
>     (Nothing, False, _) -> (,,) Nothing h <$> (damn "Wait")
>     (_, True, _) -> damn "" >> return (Nothing, h, "")


> type AQS = (Int, Maybe V, Program)

> hLabel :: String -> StateT AQS Mo (Html, [(Label, V)])
> hLabel k = do
>   (n, mv, cs) <- get
>   case cs of
>     L l : cs' -> do
>       (mv', qh, mh) <- lift $ qhex (k ++ show n) mv
>       put (n + 1, mv', cs')
>       return (id << [td (primHtml qh), td (sub (primHtml mh)),
>                  td << (tt << primHtml (l ++ ": &nbsp; &nbsp; "))],
>              maybe [] (\ v -> [(l, v)]) mv')
>     _ -> return (id << [td noHtml, td noHtml, td noHtml], [])

> shopd :: (Label, Maybe V) -> String
> shopd (l, Nothing) = l
> shopd (l, Just v) = l ++ "+" ++ [shex v]

> hInst :: String -> StateT AQS Mo Html
> hInst k = do
>   (n, mv, cs) <- get
>   case cs of
>     I o Nothing : cs' -> do
>       (mv', qh, mh) <- lift $ qhex (k ++ show n) mv
>       put (n + 1, (1 +/) <$> mv', cs')
>       return << [td (primHtml qh), td (sub (primHtml mh)), td << (tt << show o)]
>     I o (Just u) : cs' -> do
>       (mv', qh, mh) <- lift $ qhex (k ++ show n) mv
>       put (n + 1, (2 +/) <$> mv', cs')
>       return << [td (primHtml qh), td (sub (primHtml mh)),
>        td << (tt << [show o, " ", shopd u])]
>     D v : cs' -> do
>       (mv', qh, mh) <- lift $ qhex (k ++ show n) mv
>       put (n + 1, (1 +/) <$> mv', cs')
>       return << [td (primHtml qh), td (sub (primHtml mh)), td << shex v]
>     _ -> return << [td noHtml, td noHtml, td noHtml]

> hProgRows :: String -> Program -> Mo (Html, [(Label, V)])
> hProgRows s p = evalStateT grok (0, Just 0, p) where
>   grok = do
>     (n, mv, cs) <- get
>     case cs of
>       [] -> do
>         (mv', qh, mh) <- lift $ qhex (s ++ show n) mv
>         return (tr << [td (primHtml qh), td (sub (primHtml mh))], [])
>       _ -> do
>         (lah, sy1) <- hLabel s
>         inh <- hInst s
>         (rh, sy2) <- grok
>         return (tr (lah +++ inh) +++ rh, sy1 ++ sy2)

> want :: String -> (Int, Maybe V) -> Mo Html
> want k (n, mv) = do
>   (_, qh, mh) <- qhex (k ++ show n) mv
>   return (td << [primHtml qh, sub (primHtml mh)])

> data AQMode = AssOnly | AssGuess | AssTrace

> assQ :: String -> AQMode -> Program -> [V] -> Mo String
> assQ k aqm cs inp = do
>   (ph, sy) <- hProgRows k cs
>   let vs = take 16 (assemble sy cs ++ repeat (Just 0))
>   let chs = tr << map ((td <<) . shex) [0 .. 15]
>   ct <- table <$> ((chs +++) <$>
>           ((tr <<) <$> traverse (want (k ++ "loc")) (zip [0 .. 15] vs)))
>   let rvs = take 16 (map fromJust (assemble (symTab 0 cs) cs) ++ repeat 0)
>   roq <- case aqm of
>     AssOnly -> return noHtml
>     AssTrace -> do
>       us <- foldMap (\ a -> mo . Var $ (k ++ "loc" ++ show a)) [0..15]
>       if us /= map ("0123456789ABCDEF" !!) rvs
>         then abuse <<  [  "The rest of the question will appear when "
>                        ,  "the answer is correct this far."
>                        ]
>         else primHtml <$> traceQ k (TINY 0 0 0 0 rvs) inp
>     AssGuess -> do
>       let oup = readOut . snd . fst $ tiny (TINY 0 0 0 0 rvs) inp
>       (og, oh) <- textField (k ++ "oup") (length oup + 2)
>       om <- case filter isAlphaNum og of
>         s | s == map shex oup -> qpraise << "Yes!"
>         "" -> abuse << ""
>         s | length s == length oup -> abuse << "Check your working!"
>         _ -> abuse << "Guess again!"
>       return << p <<
>         [  id << "You should be able to "
>         ,  id << "figure out what the program does without tracing in "
>         ,  id << "detail. Suppose the input queue is ",
>            (if null inp then italics << "empty" else (tt << map shex inp))
>         ,  id << ": what is the output? ", primHtml oh, om
>         ]
>   return . renderHtmlFragment <<
>      [  p <<
>         [  "First, size up the program! Mark the boxes in this program "
>         ,  "with the hexadecimal address of the next location to be used. "
>         ,  "If memory fills up, go round to 0 (and hope the program is "
>         ,  "over)! "
>         ,  "This should show you the address "
>         ,  "corresponding to each label."
>         ]
>      ,  p (table << ph)
>      ,  p <<
>         [  "Now that you know the address corresponding to each label, "
>         ,  "you should be able to assemble the program. Load the program "
>         ,  "into memory, below, starting at location 0. Be sure to fill "
>         ,  "up any spare memory with 0s."
>         ]
>      ,  p ct
>      ,  roq
>      ]

> chk :: [(Label, V)] -> [V] -> [(Program, Html, Html)] -> Mo Html
> chk tab [] [] = return noHtml
> chk tab vs [] = (tr <<) <$> ((td <<) <$>
>  (abuse << [id << "Whatever happened to ", tt << map shex vs, id << "?"]))
> chk tab vs ((cs, s, a) : lces) =
>   let us = assemble tab cs
>       crawl vs [] = Right vs
>       crawl (v : vs) (Just u : us) | v == u = crawl vs us
>       crawl vs us = Left (drop (length us) vs)
>   in  case crawl vs us of
>         Right vs1 -> do
>           sh <- qpraise s
>           p <- qpraise << map (maybe '.' shex) us
>           r <- chk tab vs1 lces
>           return << ((tr << [  td << [sh, id << " ", a]
>                            ,  td << [primHtml " ; makes ", tt p] ] ) +++ r)
>         Left vs1 -> do
>           sh <- qpraise s
>           x  <- abuse << map (maybe '?' shex) us
>           r <- chk tab vs1 lces
>           return << ((tr << [  td << [sh, id << " ", a]
>                            ,  td << [primHtml " ; makes ", tt x] ] ) +++ r)


> dataM :: Program -> Program -> Mo Html
> dataM [] [] = pure noHtml
> dataM (L _ : vs) us = dataM vs us
> dataM vs (L _ : us) = dataM vs us
> dataM (I _ Nothing : vs) (I _ Nothing : us) = dataM vs us
> dataM (I _ (Just _) : vs) (I _ (Just _) : us) = dataM vs us
> dataM (D _ : vs) (D _ : us) = dataM vs us
> dataM (I _ _ : _) (D v : _) =
>   abuse << [  id << "You have a ", tt << shex v, id << " as data, "
>            ,  id << "but I think it's part of an instruction."
>            ]
> dataM (D _ : _) (I o _ : _) =
>   abuse << [  id << "You have a ", tt << show o, id << " instruction "
>            ,  id << "which I don't think can be executed."
>            ]
> dataM _ _ = abuse << "Something's out of alignment!"

> diagnose :: Diagnostic Char -> String
> diagnose Clueless = "I got lost near "
> diagnose (Thing w "") = "I found " ++ w ++
>   " but I got stuck at "
> diagnose (Thing w s) = "I thought I had " ++ w ++ " beginning with " ++ s ++
>   " but I got stuck at "

> disQ :: String -> Program -> [V] -> Mo String
> disQ k cs inp = do
>   let vs = take 16 (map fromJust (assemble (symTab 0 cs) cs) ++ repeat 0)
>   let mem = vs >>= \ v -> [' ', shex v]
>   (g, h) <- textArea (k ++ "ass") 17 40  ("start: " ++ mem)
>   let pls = map (exParse program) (lines g)
>   lces <- for pls $ \ (s, u, r) ->
>     case u of
>       Right cs -> pure (cs, tt << grrr s, noHtml)
>       Left e -> (,,) [] (tt << grrr s)
>         <$> (abuse << [id << diagnose e, id << " ", tt << r])
>   let ucs = (foldMap (\ (cs, _ , _) -> cs) lces)
>   let tab = symTab 0 ucs
>   ph <- table <$> chk tab vs lces
>   dh <- dataM cs ucs
>   let oup = readOut . snd . fst $ tiny (TINY 0 0 0 0 vs) inp
>   let oul = length oup + 2
>   (og, oh) <- textField (k ++ "oup") (length (show oul))
>   om <- case filter isAlphaNum og of
>     s | s == map shex oup -> qpraise << "Yes!"
>     "" -> abuse << ""
>     s | length s == length oup -> abuse << "Check your working!"
>     _ -> abuse << "Guess again!"
>   return . renderHtmlFragment <<
>     [  p << "Your mission is to disassemble this block of machine code:"
>     ,  blockquote << (tt << mem)
>     ,  p <<
>        [  "Write your assembly language version in this box. "
>        ,  "I've started by guessing it's all data, but you should "
>        ,  "try to express the parts which get executed as instructions."
>        ]
>     ,  table << (tr << [  td ! [valign "top"] << primHtml h
>                        ,  td ! [valign "top"] << [ph, br, dh] ])
>     ,  p <<
>        [  id << "You should eventually be able to "
>        ,  id << "figure out what the program does without tracing in "
>        ,  id << "detail. Suppose the input queue is ", tt << map shex inp
>        ,  id << ": what is the output? ", primHtml oh, om
>        ]
>     ]
