> {-# LANGUAGE ExistentialQuantification #-}

> module Q9 where

> import Control.Monad
> import Control.Applicative
> import Data.Foldable hiding (elem, all, foldl, concat, sum, any)
> import Data.Traversable
> import Data.Maybe
> import Data.Monoid
> import Data.Char
> import Data.List
> -- import Network.CGI
> import Text.XHtml

> import Sig
> import QuizUtils
> import HackOldQuiz
> -- import ExParse


> data Op = PLUS | MINUS | TIMES deriving Eq

> instance Show Op where
>   show PLUS = "+"
>   show MINUS = "-"
>   show TIMES = "*"

> data RT x
>   = O x
>   | forall w. RT (w -> x) :$ RT w
>   | N [RT x]

> tpik :: RT x -> Mo x
> tpik (O x) = return x
> tpik (f :$ p) = tpik f <*> tpik p
> tpik (N ps) = do
>   i <- mo $ Rnd (0, length ps - 1)
>   tpik (ps !! i)

> nbspify :: String -> Html
> nbspify = tt . primHtml . foldMap nb where
>   nb ' ' = "&nbsp;"
>   nb c = [c]

> data BE
>   = BB Bool
>   | BF BE
>   | BV String
>   | BN BE
>   | BO BE Op BE
>   | BA String [BE]
>   deriving Eq

> bshow :: Bool -> String
> bshow True   = "1"
> bshow False  = "0"

> instance Show BE where
>   showsPrec p (BB b) = (bshow b ++)
>   showsPrec p (BF e) = ("[" ++) . showsPrec 0 e . ("]" ++)
>   showsPrec p (BA f es) = (f ++) . ("(" ++) . blat es . (")" ++) where
>     blat []        = id
>     blat [e]       = showsPrec 0 e
>     blat (e : es)  = showsPrec 0 e . ("," ++) . blat es
>   showsPrec p (BV x) = (x ++)
>   showsPrec p (BN e) = ("!" ++) . showsPrec 6 e
>   showsPrec p (BO e1 PLUS e2) | p < 4 =
>     showsPrec 4 e1 . (" + " ++) . showsPrec 4 e2
>   showsPrec p (BO e1 TIMES e2) | p < 5 =
>     showsPrec 6 e1 . (" * " ++) . showsPrec 5 e2
>   showsPrec p e = ("(" ++) . showsPrec 0 e . (")" ++)

> newtype LP x = LP {runLP :: String -> (Int, ES (x, String))}
> instance Monad LP where
>   return x = LP $ \ s -> (0, OK (x, s))
>   LP pa >>= k = LP $ \ s0 -> case pa s0 of
>     (m, Err e) -> (m, Err e)
>     (m, OK (a, s1)) -> case runLP (k a) s1 of
>       (n, Err e) -> (m + n, Err e)
>       (n, OK (b, s2)) -> (m + n, OK (b, s2))
> instance Applicative LP where
>   pure = return
>   (<*>) = ap
> instance Functor LP where
>   fmap = (<*>) . pure
> instance Alternative LP where
>   empty = LP $ \ s -> (0, Err "")
>   LP p <|> LP q = LP $ \ s -> case p s of
>     (m, Err e) -> case q s of
>       (n, Err f)
>         | m > n -> (m, Err e)
>         | n > m -> (n, Err f)
>         | length e >= length f ->  (m, Err e)
>         | otherwise -> (n, Err f)
>       r -> r
>     r -> r
> ch :: (Char -> Bool) -> String -> LP Char
> ch p w = LP $ \ s -> case s of
>   (x : xs) -> if p x then (1, OK (x, xs)) else
>     (0, Err ("'" ++ x : "' unexpected! Wanted " ++ w ++ "!"))
>   [] -> (0, Err ("Wanted '" ++ w ++ "' but found nothing!"))
> cq :: Char -> LP ()
> cq c = ch (c ==) ("'" ++ c : "'") *> pure ()
> lspc :: LP ()
> lspc = many (ch isSpace "space") *> pure ()
> int :: LP Int
> int = lspc *> (negate <$ cq '-' <*> pos <|> pos) where
>   pos = read <$> some (ch isDigit "a digit")
> chk :: Alternative a => Bool -> a ()
> chk True  = pure ()
> chk False = empty

> nom :: LP String
> nom =  ((:)  <$> ch isLower "a lower case letter"
>              <*> many (ch isAlphaNum "an alphanumeric"))
> bep :: Int -> LP BE
> bep p = lspc *> (
>        id <$ cq '(' <*> bep 0 <* cq ')'
>   <|>  BF <$ cq '[' <*> bep 0 <* cq ']'
>   <|>  BA <$> nom <* lspc <* cq '(' <*> csep (lspc *> bep 0 <* lspc) <* cq ')'
>   <|>  BV <$> nom
>   <|>  BB True <$ cq '0'
>   <|>  BB False <$ cq '1'
>   <|>  BN <$ cq '!' <* lspc <*> bep 6
>   ) >>= bec p

> csep :: LP p -> LP [p]
> csep p = (:) <$> p <*> many (lspc *> cq ',' *> lspc *> p) <* lspc <|> [] <$ lspc

> bec :: Int -> BE -> LP BE
> bec p e = lspc *> (
>   ((BO e <$ chk (p < 4) <*> (PLUS <$ cq '+') <*> bep 4)
>     >>= bec p)
>   <|>
>   ((BO e <$ chk (p < 5) <*> (TIMES <$ cq '*') <*> bep 5) >>= bec p)
>   <|>
>   pure e)

> instance Read BE where
>   readsPrec p s = case runLP (bep 0) s of
>     (_, Err _) -> []
>     (_, OK xs) -> [xs]

> yuk :: ES x -> String
> yuk (Err s) = s
> yuk _ = "From here on, I'm lost!"

> doop :: Op -> Int -> Int -> Int
> doop PLUS 0 x = x
> doop PLUS x _ = x
> doop TIMES 0 _ = 0
> doop TIMES _ x = x

> noo :: Int -> Int
> noo 0 = 1
> noo _ = 0

> ttr ::  Int -> String -> [(String, [Int] -> Maybe Int)] -> [(String, Int)] ->
>         BE -> Mo (Maybe Int, Html)
> ttr p k fs g (BV x) = do
>   return (lookup x g, td << noHtml)
> ttr p k fs g (BO e PLUS f) | p < 4 = do
>   (ne, he) <- ttr 4 (k ++ "l") fs g e
>   (nf, hf) <- ttr p (k ++ "r") fs g f
>   (r, h, m) <- numBox k (doop PLUS <$> ne <*> nf)
>   return (r, he +++ (td << [primHtml h, sub (primHtml m)]) +++ hf)
> ttr p k fs g (BO e TIMES f) | p < 5 = do
>   (ne, he) <- ttr 5 (k ++ "l") fs g e
>   (nf, hf) <- ttr p (k ++ "r") fs g f
>   (r, h, m) <- numBox k (doop TIMES <$> ne <*> nf)
>   return (r, he +++ (td << [primHtml h, sub (primHtml m)]) +++ hf)
> ttr p k fs g (BN e) = do
>   (ne, he) <- ttr 6 (k ++ "l") fs g e
>   (r, h, m) <- numBox k (noo <$> ne)
>   return (r, (td << [primHtml h, sub (primHtml m)]) +++ he)
> ttr p k fs g (BA f es) = do
>   nhs <- traverse (\ (i, e) -> ttr 0 (k ++ show i) fs g e) (zip [0..] es)
>   let v = do {h <- lookup f fs; is <- traverse fst nhs; h is}
>   (r, h, m) <- numBox k v
>   return (r, (td << [primHtml h, sub (primHtml m)]) +++ foldMap (\ (_, h) -> h +++ td noHtml) nhs)
> ttr p k fs g (BB True) = return (Just 1, td << "1")
> ttr p k fs g (BB False) = return (Just 0, td << "0")
> ttr p k fs g (BF e) = ttr p k fs g e
> ttr p k fs g e = do
>   (r, h) <- ttr 0 k fs g e
>   return (r, (td << noHtml) +++ h +++ (td << noHtml))

> tth :: Int -> BE -> Html
> tth _ (BV x) = td << x
> tth p (BO e PLUS f) | p < 4
>   = tth 4 e +++ (td ! [align "center"] << "+") +++ tth p f
> tth p (BO e TIMES f) | p < 5
>   = tth 5 e +++ (td ! [align "center"] << "*") +++ tth p f
> tth p (BN e) = (td ! [align "center"] << "!") +++ tth 6 e
> tth _ (BA f []) = td << (f ++ "()")
> tth _ (BA f es) = td << (f ++ "(") +++ bla es where
>   bla [e] = tth 0 e +++ (td << ")")
>   bla (e : es) = tth 0 e +++ (td << ",") +++ bla es
> tth p (BB True) = td << "1"
> tth p (BB False) = td << "0"
> tth p (BF e) = tth p e
> tth p e = (td << "(") +++ tth 0 e +++ (td << ")")

> bvars :: BE -> [String]
> bvars = nub . vars where 
>   vars (BV v) = [v]
>   vars (BO e _ f) = vars e ++ vars f
>   vars (BN e) = vars e
>   vars (BF e) = vars e
>   vars (BA _ es) = foldMap vars es
>   vars _ = []

> ttl :: String -> [String] -> Mo ([(String, Int)], Html)
> ttl k = foldMap $ \ x -> do
>   (s, h) <- qfield (k ++ x) "" [thetype "text", size "1", maxlength 1]
>   case (filter (not . isSpace) s) of
>     "0" -> return ([(x, 0)], td << h)
>     "1" -> return ([(x, 1)], td << h)
>     _ -> (abuse << "Bit?") >>= \ a -> return ([], td << [h, sub a])

> pow2 :: Int -> Int
> pow2 0 = 1
> pow2 n = 2 * pow2 (n - 1)

> envs :: [String] -> [[(String, Int)]]
> envs [] = [[]]
> envs (x : xs) = (:) <$> ((,) x <$> [0, 1]) <*> envs xs

> ttq :: String -> String -> Mo String
> ttq k es = do
>   let e = read es
>   let fs = [("xor", Just . (`mod` 2) . sum)]
>   let vs = bvars e
>   let hh = tr (foldMap (td <<) vs +++ (td << primHtml "&nbsp; | &nbsp;") +++ tth 0 e)
>   ghs <- for [1 .. pow2 (length vs)] $ \ i -> do
>     (g, lh) <- ttl (k ++ "t" ++ show i) vs
>     (_, rh) <- ttr 0 (k ++ "e" ++ show i) fs g e
>     return (g, tr (lh +++ (td << primHtml "&nbsp; | &nbsp;") +++ rh))
>   let qth = table << (hh : map snd ghs)
>   let gs = sort (map (sort . fst) ghs)
>   let ws = sort (map sort (envs vs))
>   m <- case [() | w <- ws, not (elem w gs)] of
>     [] -> qpraise << "That's covered it!"
>     _  -> abuse << "Gotta catch 'em all!"
>   return . renderHtml $ paragraph << [qth, br, m]


> du :: Op -> Op
> du PLUS = TIMES
> du TIMES = PLUS

> data Chg = Fo | Co | Ho String deriving Show
> dmchg :: BE -> BE -> [Chg]
> dmchg (BF g@(BN (BO e o f))) (BF g'@(BO (BN e') o' (BN f')))
>   | e == e' && f == f'
>   = if o == du o' then [Co]
>       else [Ho ("When you negate " ++ show o ++
>                 ", you get " ++ show (du o) ++ ", remember?")]
>   | otherwise = [Ho ("It's a radical change from " ++ show g ++ " to " ++ show g' ++
>                      ", isn't it?")]
> dmchg (BF g@(BN (BN e))) (BF e')
>   | e == e' = [Co]
>   | otherwise = [Ho ("It's a radical change from " ++ show g ++ " to " ++ show e' ++
>                      ", isn't it?")]
> dmchg (BF e) e' = dmchg e e'
> dmchg e (BF e') = Fo : dmchg e e'
> dmchg (BB i) (BB j) | i == j = []
> dmchg (BO e0 o e1) (BO e0' o' e1') | o == o' = dmchg e0 e0' ++ dmchg e1 e1'
> dmchg (BN e) (BN e') = dmchg e e'
> dmchg (BA a es) (BA b fs) | a == b && length es == length fs
>   =  concat (zipWith dmchg es fs)
> dmchg g@(BN (BO e o f)) g'@(BO (BN e') o' (BN f')) =
>   [Ho ("Should you reduce " ++ show g ++ " to " ++ show g' ++ ", if it's not in focus?")]
> dmchg g@(BN (BN e)) e' =
>   [Ho ("Should you reduce " ++ show g ++ " to " ++ show e' ++ ", if it's not in focus?")]
> dmchg (BV x) (BV y) | x == y = []
> dmchg e e' = [Ho ("Are you sure " ++ show e ++ " gives " ++ show e' ++ " ?")]

> allnv :: BE -> Bool
> allnv (BN (BV _)) = True
> allnv (BN (BF e)) = allnv (BN e)
> allnv (BN _) = False
> allnv (BO e _ e') = allnv e && allnv e'
> allnv (BA _ es) = all allnv es
> allnv (BF e) = allnv e
> allnv _ = True

> evmark :: BE -> [String] -> Mo Html
> evmark e (l : ls) | all isSpace l = evmark e ls
> evmark e [] | allnv e = qpraise << "That's it!"
> evmark e (l : ls) = case runLP (bep 0) l of
>   (_, OK (e', "")) ->
>     let d = dmchg e e'
>         hs = [s | Ho s <- d]
>     in  case (d, hs) of
>           (_ : _ : _, []) ->
>             abuse <<
>               [  id << "You can't focus on two places!", br
>               ,  foldMap (\ x -> (tt << x) +++ br) (l : ls)
>               ]
>           (_, _ : _) ->
>             abuse <<
>               [  foldMap (+++ br) hs
>               ,  foldMap (\ x -> (tt << x) +++ br) (l : ls)
>               ]
>           (_, []) ->
>             qpraise (tt << l +++ br) `mappend` evmark e' ls       
>   (m, y) -> abuse <<
>      [  tt << l, br
>      ,  nbspify (replicate m ' '),  id << ("^^ " ++ yuk y), br
>      ,  foldMap (\ x -> (tt << x) +++ br) ls
>      ]
> evmark e [] = abuse << "OK so far! Now finish the job!"

> hard :: BE -> Int
> hard (BO x _ y) = hard x + hard y
> hard (BN e) = nard e where
>   nard (BN e) = 1 + hard e
>   nard (BO e _ f) = 1 + nard e + nard f
>   nard _ = 0
> hard (BA _ es) = sum (map hard es)
> hard _ = 0

> evprob :: String -> String -> Mo String
> evprob k es = do
>   let e = read es
>   let n = hard e
>   let s = show e
>   (eg, eh) <- textArea k (2 * n) (7 + length s) ""
>   em <- evmark e (lines eg)
>   return . renderHtml $ paragraph <<
>     [  strong << "Expression: ", id << s, br
>     ,  primHtml eh, br
>     ,  em
>     ]

> sop :: BE -> [[(String, Bool)]]
> sop (BO e PLUS f) = sop e ++ sop f
> sop (BO e TIMES f) = [a ++ b | a <- sop e, b <- sop f]
> sop (BV x) = [[(x, True)]]
> sop (BN (BV x)) = [[(x, False)]]
> sop (BN (BN e)) = sop e
> sop (BN (BO e o f)) = sop (BO (BN e) (du o) (BN f))
> sop (BN (BF e)) = sop (BN e)
> sop (BF e) = sop e
> sop (BB False) = []
> sop (BB True) = [[]]

> usop :: BE -> Either BE [[(String, Bool)]]
> usop e@(BO (BO _ PLUS _) TIMES _) = Left e
> usop e@(BO _ TIMES (BO _ PLUS _)) = Left e
> usop (BN (BV x)) = Right [[(x, False)]]
> usop e@(BN _) = Left e
> usop (BV x) = Right [[(x, True)]]
> usop (BB False) = Right []
> usop (BB True) = Right [[]]
> usop (BO e PLUS f) = case (usop e, usop f) of
>   (Right cs, Right cs') -> Right (cs ++ cs')
>   (Left e, _) -> Left e
>   (_, Left e) -> Left e
> usop d@(BO e TIMES f) = case (usop e, usop f) of
>   (Right [], Right _) -> Right []
>   (Right _, Right []) -> Right []
>   (Right [c], Right [c']) -> Right [c ++ c']
>   _ -> Left d
> usop e = Left e

> dom :: [(String, Bool)] -> [(String, Bool)] -> Bool
> dom xs ys = all (`elem` ys) xs

> sim :: [(String, Bool)] -> [[(String, Bool)]]
> sim [] = [[]]
> sim ((x, b) : xbs) = case (filter (== (x, not b)) xbs, filter (/= (x, b)) xbs) of
>   ([], xbs') -> [(x, b) : ybs | ybs <- sim xbs']
>   _ -> []

> dommers :: [[(String, Bool)]] -> [[(String, Bool)]]
> dommers [] = []
> dommers (c : cs) = case dommers cs of
>   ds | any (`dom` c) ds -> ds
>   ds -> c : filter (not . dom c) ds

> sopdnf :: [[(String, Bool)]] -> [[(String, Bool)]]
> sopdnf = dommers . foldMap sim

> csops :: [[(String, Bool)]] -> [[(String, Bool)]] -> [([(String, Bool)], Bool)]
> csops gs ws = [(g, False) | g <- gs, not (elem (sort g) wss)] ++
>               [(w, True) | w <- ws, not (elem (sort w) gss)]
>   where
>     gss = sort (map sort gs)
>     wss = sort (map sort ws)

> claush :: [(String, Bool)] -> String
> claush [] = "1"
> claush [(x, True)] = x
> claush [(x, False)] = "!" ++ x
> claush (l : ls) = claush [l] ++ "*" ++ claush ls

> dnfq :: String -> String -> Mo String
> dnfq k es = do
>   let e = read es
>   let s1 = sop e
>   let n = 3 * length s1 + 5 * sum (map length s1)
>   (i1, h1) <- qfield (k ++ "1") ""  [thetype "text", size (show n), maxlength n]
>   (s2, m1) <- case runLP (bep 0) i1 of
>     (_, OK (e, "")) -> case usop e of
>        Left e' -> (,) Nothing <$> (abuse << 
>          [ show e', "does not look like a disjunction of conjunctions to me!" ])
>        Right s -> (,) (Just s) <$> case csops s s1 of
>          [] -> qpraise << "Seems about right!"
>          (g, False) : _ -> abuse << ["Where did ", claush g, " come from?"]
>          (w, True) : _ -> abuse << ["Where did ", claush w, " disappear to?"]
>     (m, y) -> (,) Nothing <$> (abuse <<
>        [  tt << i1, br
>        ,  nbspify (replicate m ' '),  id << ("^^ " ++ yuk y), br ])
>   let d1 = sopdnf <$> s2
>   (i2, h2) <- qfield (k ++ "2") ""  [thetype "text", size (show n), maxlength n]
>   m2 <- case runLP (bep 0) i2 of
>     (_, OK (e, "")) -> case usop e of
>        Left e' -> abuse << 
>          [ show e', "does not look like a disjunction of conjunctions to me!" ]
>        Right d -> case d1 of
>          Nothing -> abuse << "Premature!"
>          Just d1 -> case csops d d1 of
>            [] -> if sort (map sort d) == sort (map sort d1)
>                    then qpraise << "Seems about right!"
>                    else abuse << "Have you removed all duplicates and dominators?"
>            (g, False) : _ -> abuse << ["Where did ", claush g, " come from?"]
>            (w, True) : _ -> abuse << ["Where did ", claush w, " disappear to?"]
>     (m, y) -> abuse <<
>        [  tt << i2, br
>        ,  nbspify (replicate m ' '),  id << ("^^ " ++ yuk y), br ]
>   return . renderHtml $ paragraph <<
>     [  id << "Transform the following to a disjunction of conjunctions of literals "
>     ,  id << "by distributing and (*) over or (+)"
>     ,  blockquote << show e
>     ,  id << "Don't simplify the result! (That's the next bit.)", br
>     ,  h1, br, m1, br
>     ,  id << "Now simplify your answer by removing duplicate literals "
>     ,  id << "(e.g. x*y*x becomes x*y), contradictory clauses (e.g., x*y*!x must go), "
>     ,  id << "and any clause dominated by another (e.g., x + x*y simplifies to x, because "
>     ,  id << "x*y cannot be true unless x is true already!", br
>     ,  h2, br, m2
>     ] 



> {-
> guff :: Mo Html
> guff = quiz "CS.106 Quiz " "9 Boolean Logic" <$> do
>   tt1 <- ttq "tt1" [] (read "(x+y)*!(x*y)")
>   tt2 <- ttq "tt2" [] (read "x*!y+y*!x")
>   tt3 <- ttq "tt3" [("xor", Just . (`mod` 2) . sum)] (read "xor(x, xor(y, z))")
>   tt4 <- ttq "tt4" [("xor", Just . (`mod` 2) . sum)] (read "xor(xor(x, y), z)")
>   ev1 <- evprob "ev1" (read "!((x+!y)*(y+!x))")
>   ev2 <- evprob "ev2" (read "!(x*y+!x*!y)")
>   dp1 <- dnfq "dp1" (read "(x + y) * (!x + !y)")
>   dp2 <- dnfq "dp2" (read "(x + !y) * (!x + y)")
>   dp3 <- dnfq "dp3" (read "(x + y) * (x + z)")
>   (sb, paf) <- qsub
>   return $ fold
>     [  qpart ceQ "parta" "(a) Cooking xor both ways" <<
>        [  paragraph <<
>             [  id << "Here are two different definitions of 'exclusive or'."
>             ,  table << [tr << [td << blockquote <<
>                [  thecode << "xor :: (Bit, Bit) -> Bit", br
>                ,  thecode << "xor(x,y) = (x + y) * !(x * y)"
>                ]
>             ,  td << "versus"
>             ,  td <<blockquote <<
>                [  thecode << "xor :: (Bit, Bit) -> Bit", br
>                ,  thecode << "xor(x,y) = x * !y + y * !x"
>                ]]]
>             ,  id << "Use truth tables to check that these definitions are equivalent."
>             ]
>        ,  table ! [border 1] << tr << [td << tt1, td << tt2]
>        ,  paragraph << sb
>        ]
>     ,  qpart ceQ "partb" "(b) An Exclusive Association" <<
>        [  paragraph <<
>             [  id << "Now that you know how xor works, check that it is associative!"
>             ]
>        ,  table ! [border 1] << tr << [td << tt3, td << tt4]
>        ,  paragraph << sb
>        ]
>     ,  qpart ceQ "partc" "(c) Focus and Negate" <<
>        [  paragraph <<
>           [  id << "Here are some logical expressions. "
>           ,  id << "Please give a trace showing how to push negation to "
>           ,  id << "the variables, focusing on one subexpression at a time, "
>           ,  id << "and applying either one of de Morgan's laws "
>           ,  blockquote << [  id << "!(a * b) = !a + !b", br
>                            ,  id << "!(a + b) = !a * !b"]
>           ,  id << "or double negation elimination"
>           ,  blockquote << "!!b = b"
>           ,  id << "Mark the expression in focus with square brackets [..]. "
>           ,  id << "At each step, you should either calculate at the focus "
>           ,  id << "or move the focus (preferably to a place that's ready "
>           ,  id << "for calculation), but not both."
>           ]
>        ,  table ! [cellspacing 10] <<
>             tr ! [valign "top"] << [td ev1, td ev2]
>        ,  sb
>        ,  paragraph <<
>           [  strong << "Example expression: ", id << "!(!a * (b + c))", br
>           ,  blockquote << table <<
>              [  tr <<
>                 [th << "What I need to write in the box"
>                 , th << "What I'm thinking"]
>              ,  tr <<
>                 [  td << thecode << "[!(!a * (b + c))]"
>                 ,  td << italics <<
>                     "only one ! is not at a variable already"
>                 ]
>              ,  tr <<
>                 [  td << thecode << "[!!a + !(b + c)]"
>                 ,  td << italics << "de Morgan turns not-and to or-nots"
>                 ]
>              ,  tr <<
>                 [  td << thecode << "[!!a] + !(b + c)"
>                 ,  td << italics << "focus on !!a (could have picked the other negation)"
>                 ]
>              ,  tr <<
>                 [  td << thecode << "[a] + !(b + c)"
>                 ,  td << italics << "double negation goes"
>                 ]
>              ,  tr <<
>                 [  td << thecode << "a + [!(b + c)]"
>                 ,  td << italics << "refocus to the other negation"
>                 ]
>              ,  tr <<
>                 [  td << thecode << "a + [!b * !c]"
>                 ,  td << italics << "de Morgan turns nor-or to and-nots"
>                 ]
>              ]
>           ]
>        ]
>     ,  qpart ceQ "partd" "(d) Distribute and simplify" <<
>        [  dp1 , dp2 , dp3
>        ,  paragraph << sb
>        ]
>     ,  h2 << "And the outcome..."
>     ,  p << paf
>     ,  p << anchor ! [href "My106.cgi"] << "Back to the CS.106 battlefield!"
>     ]
> -}