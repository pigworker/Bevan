> module TINY where

> import Data.Word
> import Data.Bits
> import Data.Char
> import Data.List
> import Control.Monad.Except
> import Control.Monad.Trans.Except
> import Control.Monad.State
> import Control.Applicative
> import Data.Traversable
> import Data.Foldable hiding (concat, all)

> import ExParse

> type Parser = ExParse Char

> type V = Int   -- grrr

> data Op  = HLT  | JMP  | JZE  | JNZ
>          | LDA  | STA  | GET  | PUT
>          | ROL  | ROR  | ADC  | CCF
>          | SCF  | DEL  | LDL  | FLA
>          deriving (Show, Eq, Ord, Enum, Read)

> data TINY = TINY
>   {  ip   :: V
>   ,  li   :: V
>   ,  fr   :: V
>   ,  ac   :: V
>   ,  mem  :: [V]
>   }  deriving (Eq, Show)

> data Outcome
>   = Starved
>   | Looping
>   | Halted
>   deriving (Show, Eq)

instance Error Outcome where
  noMsg = Halted

> type TM = StateT (TINY, [V]) (Except Outcome)

> type TrOut = (Op, Maybe V, Maybe V, Maybe V)

> m16 :: V -> V
> m16 v = mod v 16

> rd :: V -> TM V
> rd a = do
>   (t, _) <- get
>   return $ mem t !! (mod a 16)

> update :: [V] -> V -> V -> [V]
> update (_ : xs) 0 v  = v : xs
> update (x : xs) i v  = x : update xs (i - 1) v

> ($=) :: V -> V -> TM ()
> a $= v = do
>   (t, is) <- get
>   put (t {mem = update (mem t) (m16 a) (m16 v)}, is)

> ipv :: TM V
> ipv = do
>   i <- gip
>   v <- rd i
>   pip (i + 1)
>   return v

> tweak :: Bool -> Int -> V -> V
> tweak b i v = (if b then setBit else clearBit) v i

> gip :: TM V
> gip = do
>   (t, _) <- get
>   return (ip t)

> pip :: V -> TM ()
> pip a = do
>   (t, is) <- get
>   put (t {ip = m16 a}, is)

> gac :: TM V
> gac = do
>   (t, _) <- get
>   return (ac t)

> pac :: V -> TM ()
> pac a = do
>   (t, is) <- get
>   let a' = m16 a
>   put (t {ac = a'}, is)
>   pzf (a' == 0)

> gli :: TM V
> gli = do
>   (t, _) <- get
>   return (li t)

> pli :: V -> TM ()
> pli a = do
>   (t, is) <- get
>   let a' = m16 a
>   put (t {li = a'}, is)
>   pzf (a' == 0)

> gcf :: TM Bool
> gcf = do
>   (t, _) <- get
>   return $ testBit (fr t) 0

> pcf :: Bool -> TM ()
> pcf b = do
>   (t, is) <- get
>   put (t {fr = tweak b 0 (fr t)}, is)

> gzf :: TM Bool
> gzf = do
>   (t, _) <- get
>   return $ testBit (fr t) 1

> pzf :: Bool -> TM ()
> pzf b = do
>   (t, is) <- get
>   put (t {fr = tweak b 1 (fr t)}, is)

> gof :: TM Bool
> gof = do
>   (t, _) <- get
>   return $ testBit (fr t) 2

> pof :: Bool -> TM ()
> pof b = do
>   (t, is) <- get
>   put (t {fr = tweak b 2 (fr t)}, is)

> ghf :: TM Bool
> ghf = do
>   (t, _) <- get
>   return $ testBit (fr t) 3

> phf :: Bool -> TM ()
> phf b = do
>   (t, is) <- get
>   put (t {fr = tweak b 3 (fr t)}, is)

> hlt :: TM TrOut
> hlt = do
>   phf True
>   return (HLT, Nothing, Nothing, Nothing)

> jmp :: V -> TM TrOut
> jmp a = do
>   pip a
>   return (JMP, Just a, Nothing, Nothing)

> jze :: V -> TM TrOut
> jze a = do
>   z <- gzf
>   if z then pip a else return ()
>   return (JZE, Just a, Nothing, Nothing)

> jnz :: V -> TM TrOut
> jnz a = do
>   z <- gzf
>   if not z then pip a else return ()
>   return (JNZ, Just a, Nothing, Nothing)

> lda :: V -> TM TrOut
> lda a = do
>   v <- rd a
>   pac v
>   return (LDA, Just a, Nothing, Nothing)

> sta :: V -> TM TrOut
> sta a = do
>   v <- gac
>   a $= v
>   return (STA, Just a, Nothing, Nothing)

> gett :: TM TrOut
> gett = do
>   (_, is) <- get
>   case is of
>     []      -> throwError Starved
>     i : is  -> do
>       pzf (i == 0)
>       pac i
>       (t, _) <- get
>       put (t, is)
>       return (GET, Nothing, Just i, Nothing)

> putt :: TM TrOut
> putt = do
>   o <- gac
>   (t, is) <- get
>   put (t, is)
>   return (PUT, Nothing, Nothing, Just o)

> rol :: TM TrOut
> rol = do
>   c <- gcf
>   a <- gac
>   let c' = testBit a 3
>   let a' = m16 (2 * a + if c then 1 else 0)
>   pcf c'
>   pof $ testBit a 2 /= c'
>   pac a'
>   return (ROL, Nothing, Nothing, Nothing)

> ror :: TM TrOut
> ror = do
>   c <- gcf
>   a <- gac
>   let a' = div a 2 + if c then 8 else 0
>   pcf $ testBit a 0
>   pof $ testBit a 3 /= c
>   pac a'
>   return (ROR, Nothing, Nothing, Nothing)

> twoc :: V -> Int
> twoc v = case m16 v of
>   i | i >= 8     -> i -16
>     | otherwise  -> i

> adc :: V -> TM TrOut
> adc j = do
>   c <- gcf
>   a <- gac
>   b <- rd j
>   let s = twoc a + twoc b + if c then 1 else 0
>   pcf $ testBit (a + b + if c then 1 else 0) 4
>   pof $ s < -8 || s > 7
>   let a' = m16 s
>   pac a'
>   return (ADC, Just j, Nothing, Nothing)

> ccf :: TM TrOut
> ccf = pcf False >> return (CCF, Nothing, Nothing, Nothing)

> scf :: TM TrOut
> scf = pcf True >> return (SCF, Nothing, Nothing, Nothing)

> del :: TM TrOut
> del = do
>   l <- gli
>   pli (m16 (l - 1))
>   return (DEL, Nothing, Nothing, Nothing)

> ldl :: V -> TM TrOut
> ldl a = do
>   v <- rd a
>   pli v
>   return (LDL, Just a, Nothing, Nothing)

> fla :: TM TrOut
> fla = do
>   v <- gac
>   pac (15 - v)
>   return (FLA, Nothing, Nothing, Nothing)


> step :: TM TrOut
> step = do
>   h <- ghf
>   if h then throwError Halted else return ()
>   o <- ipv
>   case toEnum o of
>     HLT -> hlt
>     JMP -> jmp =<< ipv
>     JZE -> jze =<< ipv
>     JNZ -> jnz =<< ipv
>     LDA -> lda =<< ipv
>     STA -> sta =<< ipv
>     GET -> gett
>     PUT -> putt
>     ROL -> rol
>     ROR -> ror
>     ADC -> adc =<< ipv
>     CCF -> ccf
>     SCF -> scf
>     DEL -> del
>     LDL -> ldl =<< ipv
>     FLA -> fla

> looping :: TINY -> [(TINY, TrOut)] -> Bool
> looping t [] = False
> looping t ((_, (GET, _, _, _)) : _) = False
> looping t (l@(t', _) : hs) = t == t' || looping t hs

> tinyLoop :: [(TINY, TrOut)] -> TM (Outcome, [(TINY, TrOut)])
> tinyLoop ths = do
>   h <- ghf
>   (t, _) <- get
>   if looping t ths
>     then return (Looping, reverse ths)
>     else do
>       grr <- catchError (Right <$> step) (return . Left)
>       case grr of
>         Right h -> tinyLoop ((t, h) : ths)
>         Left o -> return (o, reverse ths)

> tiny :: TINY -> [V] -> ((Outcome, [(TINY, TrOut)]), TINY)
> tiny t is = case runExcept (runStateT (tinyLoop []) (t, is)) of
>   Right (ot, (t, _))  -> (ot, t)
>   Left o              -> ((o, []), t)

> hexP :: Parser V
> hexP  =    "a hex digit" $? (spc *>
>       (    (read . (:"")) <$> eat isDigit
>       <|>  10 <$ toks "A"  <|> 11 <$ toks "B"  <|> 12 <$ toks "C"
>       <|>  13 <$ toks "D"  <|> 14 <$ toks "E"  <|> 15 <$ toks "F"
>       ))

> shex :: V -> Char
> shex v = "0123456789ABCDEF" !! v

> shtiny :: TINY -> String
> shtiny  (TINY ip li fr ac
>           [m0, m1, m2, m3, m4, m5, m6, m7, m8, m9, mA, mB, mC, mD, mE, mF]) =
>   [  shex ip, ' ', shex li, ' ', shex fr, ' ', shex ac
>   ,  ' ', ' '
>   ,  shex m0, shex m1, shex m2, shex m3
>   ,  shex m4, shex m5, shex m6, shex m7
>   ,  shex m8, shex m9, shex mA, shex mB
>   ,  shex mC, shex mD, shex mE, shex mF
>   ]

> opP :: Parser Op
> opP   = "an operator" $? (
>   HLT <$ toks "HLT"  <|>  JMP <$ toks "JMP"  <|>
>   JZE <$ toks "JZE"  <|>  JNZ <$ toks "JNZ"  <|>
>   LDA <$ toks "LDA"  <|>  STA <$ toks "STA"  <|>
>   GET <$ toks "GET"  <|>  PUT <$ toks "PUT"  <|>
>   ROL <$ toks "ROL"  <|>  ROR <$ toks "ROR"  <|>
>   ADC <$ toks "ADC"  <|>  CCF <$ toks "CCF"  <|>
>   SCF <$ toks "SCF"  <|>  DEL <$ toks "DEL"  <|>
>   LDL <$ toks "LDL"  <|>  FLA <$ toks "FLA"  )

> tinyP :: TINY -> Parser TINY
> tinyP (TINY ip li fr ac mem) = "a configuration" $? (
>   TINY  <$>  (ip <$ spc <* toks "." <|> hexP) 
>         <*>  (li <$ spc <* toks "." <|> hexP)
>         <*>  (fr <$ spc <* toks "." <|> hexP)
>         <*>  (ac <$ spc <* toks "." <|> hexP)
>         <*>  traverse (\ v -> v <$ spc <* toks "." <|> hexP) mem
>         <* spc )

>   -- TODO: why are there two trailing spaces?
>   -- (copied from traceQ)
>   -- I have deleted them!
> header1 = "I L F A  Memory----------  Action---"
> header2 = "P I R C  0123456789ABCDEF  OPR & ? !"
> blank = ". . . .  ................  ### - - -"

> data TraceLine = Head1 | Head2 | Blank | TinyOp TINY TrOut | TinyBlankOp TINY | TinyNoOp TINY | Bad String (Diagnostic Char) String deriving Show
> isValid :: TINY -> Bool
> isValid (TINY ip li fr ac mem) = all (/=(-1)) $ ip:li:fr:ac:mem

> -- we are very forgiving of spacing
> -- NOTE: this may return a invalid tiny (some values -1), meaning a blank (i.e. '.') was given
> -- in the trace before an actual value.
> parseTrace :: [String] -> [TraceLine]
> parseTrace = go inval
>   where inval = TINY (-1) (-1) (-1) (-1) (replicate 16 (-1))
>         go _ [] = []
>         go t (s:ss) = case exParse (parseRow t) s
>                       of (_,Right l,_) -> l : go (newTiny t l) ss
>                          (l,Left err,r) -> Bad l err r : go t ss
>         newTiny _ (TinyOp t' _) = t'
>         newTiny _ (TinyBlankOp t') = t'
>         newTiny _ (TinyNoOp t') = t'
>         newTiny t _ = t
>
> -- as tinyP is forgiving of spacing, we need to be for header and blanks
> -- tokSpcs is like tok, but doesn't care about spacing
> parseRow :: TINY -> Parser TraceLine
> parseRow t = Head1 <$ (toksSpcs header1) <* spc <* eoi
>          <|> Head2 <$ (toksSpcs header2) <* spc <* eoi
>          <|> Blank <$ (toksSpcs blank) <* spc <* eoi
>          <|> TinyOp <$> (tinyP t) <*> troutP <* eoi
>          <|> TinyBlankOp <$> (tinyP t) <* toks "###" <* mhexP <* mhexP <* mhexP <* spc <* eoi
>          <|> TinyNoOp <$> (tinyP t) <* eoi
> --          <|> Bad <$> many (eat (const True))

> -- TODO: this will put a '.' if a reg/memory is written but keeps the same value
> showTrace :: [(TINY, TrOut)] -> TINY -> String
> showTrace tios to = let trace = map (\(ti, tro) -> shtiny ti ++ "  " ++ shtrout tro) tios
>                         final = shtiny to
>                     in intercalate "\n" $ dotify $ trace ++ [final]
>   where dotify [] = []
>         dotify (s:ss) = s : dot s ss
>         dot _ [] = []
>         dot s (t:ts) = zipWith3 d [0..] s t : dot t ts
>         d n old new | n`elem`([0,2,4,6]++[9..9+15]) && old == new = '.'
>                     | otherwise = new

> showOutcome :: Outcome -> String
> showOutcome Starved = "starved for input"
> showOutcome Looping = "loops henceforth"
> showOutcome Halted = "halted normally"

> mhexP :: Parser (Maybe V)
> mhexP =   spc *> (Nothing <$ toks "-"  <|>  Just <$> hexP)

> troutP :: Parser TrOut
> troutP = "an action" $? ((,,,) <$> opP <*> mhexP <*> mhexP <*> mhexP <* spc)

> shtrout :: TrOut -> String
> shtrout (op, a, i, o) = show op ++ [' ', mh a, ' ', mh i, ' ', mh o] where
>   mh Nothing   = '-'
>   mh (Just v)  = shex v

> checkTiny :: TINY -> TINY -> String
> checkTiny (TINY ip li fr ac mem) (TINY ip' li' fr' ac' mem') = concat
>   [  (guard (ip /= ip') >> " IP") 
>   ,  (guard (li /= li') >> " LI")
>   ,  (guard (fr /= fr') >> " FR")
>   ,  (guard (ac /= ac') >> " AC")
>   ]  ++ foldMap (\ i -> " " ++ [shex i])
>            (findIndices (uncurry (/=)) (zip mem mem'))

> checkTrOut :: TrOut -> TrOut -> String
> checkTrOut (f,a,i,o) (f', a', i', o') = concat
>   [  (guard (f /= f') >> " operator") 
>   ,  (guard (a /= a') >> " operand")
>   ,  (guard (i /= i') >> " input")
>   ,  (guard (o /= o') >> " output")
>   ]

> markConf :: TINY -> String -> TINY -> (String, String)
> markConf ti s to = case exParse (tinyP ti) s of
>   (_, Right tu, s') -> case checkTiny to tu of
>     d@(_ : _) -> ("Configuration discrepancy:" ++ d, "")
>     "" -> ("", s')
>   _ -> ("Expected TINY configuration and got " ++ s ++ ".", "")

> markLine :: Bool -> TINY -> String -> (TINY, TrOut) -> String
> markLine b ti s (to, ta) = case markConf ti s to of
>   ("", s') -> if all isSpace s' then (if b then "" else "Action?") else case exParse troutP s' of
>     (_, Right tb, _) -> case checkTrOut ta tb of
>       d@(_ : _) -> "Action discrepancy:" ++ d
>       "" -> ""
>     _ -> "Expected TINY action and got " ++ s' ++ "."
>   (s, "") -> s

> readOut :: [(TINY, TrOut)] -> [V]
> readOut = foldMap oup where
>   oup (_, (_, _, _, Just v)) = [v]
>   oup _ = []

> dullTINY :: TINY
> dullTINY = TINY 0 0 0 0 (replicate 16 0)

> ioLoopTiny :: TINY -> [V] -> ((Outcome, [(TINY, TrOut)]), TINY)
> ioLoopTiny t is = go [] where
>   go os = case tiny t (is ++ os) of
>     x@((Starved, ss), _) -> case [v | (_, (_, _, _, Just v)) <- ss] of
>       [] -> x
>       os -> go os
>     x -> x

