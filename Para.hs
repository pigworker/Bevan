module Main where

import Prelude hiding(elem, all)
import Data.Char
import Control.Monad
import Control.Applicative
import Data.Foldable hiding(foldr)
import Data.Monoid
import Data.List hiding(elem, foldr, all, foldl')

import Parse

type Attrs = [(String, String)]

taglp :: Parse Char ((Int, Bool), (String, Attrs))
taglp = do
  w <- length <$> many (ch isSpace)
  eat "<"
  b <- False <$ eat "/" <|> pure True
  n <- read <$> some (ch isDigit) <|> pure w
  t <- some (ch isAlphaNum)
  as <- if b then attrs else pure []
  punc ">"
  eoi
  return ((n, b), (t, as))

tagline :: String -> Maybe (Int, Bool, String)
tagline s = do
  (w, '<' : x) <- return $ span isSpace s
  let (b, y) = case x of
       '/' : y  -> (False, y)
       _        -> (True, x)
  let (n, z) = case span isDigit y of
       ("", z)  -> (length w, z)
       (n, z)   -> (read n, z)
  (t, '>' : r) <- return $ span isAlphaNum z
  guard $ all isSpace r
  return (n, b, t)

data PorD
  = P [String]
  | K [String]
  | D (String, Attrs) [PorD]
  deriving Show

data Bwd x = Bwd x :< x | B0 deriving Show
infixl 4 :<

-- fish
(<><) :: Bwd x -> [x] -> Bwd x
xz <>< [] = xz
xz <>< (x : xs) = (xz :< x) <>< xs
infixl 4 <><

-- chips
(<>>) :: Bwd x -> [x] -> [x]
B0 <>> xs = xs
(xz :< x) <>> xs = xz <>> (x : xs)

type Layer = (Bwd PorD, (Int, (String, Attrs)))
type Cx = (Bwd Layer, Bwd PorD, (Maybe String, Bwd String))

flushPara :: Bwd PorD -> Bwd String -> Bwd PorD
flushPara pz B0  = pz
flushPara pz sz  = pz :< P (sz <>> [])


rejig :: ((Int, Bool), (String, Attrs)) -> Bwd Layer -> Bwd PorD -> Cx
rejig a@((i, True), tas) (lz :< (qz, (j, uas))) pz
  | j >= i = rejig a lz (qz :< D uas (pz <>> []))
rejig a@((i, False), (t, _)) (lz :< (qz, (j, uas@(u, _)))) pz
  | j > i || (j == i && u == t) = rejig a lz (qz :< D uas (pz <>> []))
rejig ((i, True), tas) lz pz = (lz :< (pz, (i, tas)), B0, (Nothing, B0))
rejig _ lz pz = (lz, pz, (Nothing, B0))

glom :: Cx -> String -> Cx
glom (lz, pz, (Nothing, sz)) s
  | all isSpace s = (lz, flushPara pz sz, (Nothing, B0))
  | all (== '`') s && length s > 0 = (lz, flushPara pz sz, (Just s, B0))
  | otherwise = case parse taglp s of
     Nothing -> (lz, pz, (Nothing, sz :< s))
     Just (l, _) -> rejig l lz (flushPara pz sz)
glom (lz, pz, (Just i, sz)) s
  | s == i     = (lz, pz :< K (sz <>> []), (Nothing, B0))
  | otherwise  = (lz, pz, (Just i, sz :< (s >>= elty)))
  where
    elty c = case lookup c elemental of
      Just e -> '&' : e ++ ";"
      _ -> [c]

done :: Cx -> [PorD]
done (lz, pz, (Just i, sz)) = done (lz, pz :< K (sz <>> []), (Nothing, B0))
done (lz, pz, (Nothing, sz)) = go lz (flushPara pz sz) where
  go B0 pz = pz <>> []
  go (lz :< (qz, (_, tas))) pz = go lz (qz :< D tas (pz <>> []))

paras :: String -> [PorD]
paras = done . foldl' glom (B0, B0, (Nothing, B0)) . lines

data Chunk
  = C Char
  | E String
  | T (String, Attrs) [Chunk]
  deriving Show

escapable :: String
escapable = "\\[](){}*_?!+#@$-=`"

elemental :: [(Char, String)]
elemental = [('&', "amp"), ('<', "lt"), ('>', "gt")]

marky :: [RefDef] -> (String, String) -> Parse Char Chunk
marky rs (t, d) = T (t, []) <$ eat d <* peek (() <$ ch (not . isSpace)) <*>
  (untilp (eat d) (chunk rs) >>= \ cs -> if null cs then empty else return cs)

urlv :: Parse Char String
urlv = some (ch (\ c -> not (isSpace c || elem c ")\"'>[]")))

attrv :: Parse Char String
attrv = do
  c <- ch (`elem` "'\"")
  untilp (eat [c]) (ch (const True))

attrs :: Parse Char Attrs
attrs =
  many ((,) <$ spc <*> some (ch isLower) <* spc <* eat "=" <* spc <*> attrv) <* spc

refStuff :: Parse Char [String]
refStuff = some (id <$ spc <*> (urlv <|> attrv)) <* spc

pattor :: [RefDef] -> Parse Char [String]
pattor  rs
        =    id <$ punc "[" <*> (expand <$> some (ch isAlphaNum)) <* spc <* eat "]"
        <|>  id <$ punc "(" <*> refStuff <* eat ")"
  where expand r = maybe [] id (lookup r rs)

squarey :: [(String, (String, Maybe String, [String]))]
squarey =  [  ("!",  ("img", Just "alt",  ["src", "alt"]))
           ,  ("",   ("a",   Nothing,     ["href", "title"]))
           ]

mksq ::  (String, Maybe String, [String]) ->
         [Chunk] -> [String] -> Chunk
mksq (t, Nothing,  as) cs rs  = T (t, zip as rs) cs
mksq (t, Just a,   as) cs rs  = T (t, (a, foldr chout [] cs) : zip as rs) []

inType :: Parse Char String
inType  =    "radio" <$ eat "@"
        <|>  "checkbox" <$ eat "#"
        <|>  "submit" <$ eat "$"
        <|>  pure "text"

mkInput :: String -> String -> Int -> Chunk
mkInput "submit" co pa = T ("input", [("type", "submit"), ("value", co)]) []
mkInput t co pa = case span (/= '=') co of
  (no, '=' : va) ->
    T ("input", [("type", t), ("name", no), ("value", va)] ++
      let l = length va + pa in if l > 0 then [("size", show l)] else [])
    []
  (no, _) ->
    T ("input", [("type", t), ("name", no)] ++
       if pa > 0 then [("size", show pa)] else [])
    []

chunk :: [RefDef] -> Parse Char Chunk
chunk  rs
       =    C <$ eat "\\" <*> ch (`elem` escapable)
       <|>  T ("br", []) [] <$ eat "  \n"
       <|>  E <$ eat "&" <*> some (ch isLower) <* eat ";"
       <|>  mkInput  <$ eat "?["
             <*> inType
             <*> untilp (peek (eat "]" <|> eat "_"))
                    (eat "\\" *> ch (`elem` escapable) <|> ch (const True))
             <*> (length <$> untilp (eat "]") (eat "_"))
       <|>  foldMap (\ (h, tas) ->
                mksq tas <$ eat (h ++ "[") <*> untilp (eat "]") (chunk rs) <*> pattor rs)
              squarey
       <|>  do  n <- some (ch (== '`')) <* spc
                cs <- untilp (spc <* eat n) (C <$> ch (const True))
                return (T ("code", []) cs)
       <|>  do  eat "<"
                t <- some (ch isAlphaNum)
                as <- attrs
                T (t, as) <$>
                  (id <$ eat ">" <*> untilp (eat ("</" ++ t ++ ">")) (chunk rs)
                  <|> [] <$ eat "/>")
       <|>  do  c <- ch (const True)
                case lookup c elemental of
                  Just e   -> return (E e)
                  Nothing  -> empty
       <|>  foldMap (marky rs)
              [  ("strong",  "**")  , ("strong",  "__")
              ,  ("em",      "*")   , ("em",      "_")
              ]
       <|>  C <$> ch (const True)

plic :: Parse Char ((Int, Bool), String)
plic = (,) <$>
  ((,) <$> (length <$> many (ch isSpace)) <*>
    (True <$ some (ch isDigit) <* eat "." <|> False <$ ch (`elem` "*+-")))
  <* some (ch isSpace) <*> roi

plis :: Parse String ((Int, Bool), [String])
plis =  (\ (ib, s) ss -> (ib, s : ss)) <$> pOne plic
        <*> untilp (peek (pOne (() <$ plic)) <|> eoi) (ch (const True))

mkLis :: [((Int, Bool), [String])] -> [PorD]
mkLis [] = []
mkLis (((i, b), ss) : ls) = case span (\ ((j, _), _) -> j > i) ls of
  (ms, ns) -> D ("li", []) (P ss : if null ms then [] else [mkList ms]) : mkLis ns

mkList :: [((Int, Bool), [String])] -> PorD
mkList ls@(((_, b), _) : _) = D (if b then "ol" else "ul", []) (mkLis ls)

listy :: Parse String PorD
listy = mkList <$> some plis

ruley :: Parse String PorD
ruley = (D ("hr", []) [] <$) . pOne $ do
  l <- length <$> (some (spc <* eat "*") <|> some (spc <* eat "-")) <* spc
  guard (l >= 3)

h1under :: Parse String PorD
h1under = D ("h1", []) . return . P . return <$>
  pOne roi <* pOne (some (spc <* eat "=") <* spc)

h2under :: Parse String PorD
h2under = D ("h2", []) . return . P . return <$>
  pOne roi <* pOne (some (spc <* eat "-") <* spc)

hhash :: Parse String PorD
hhash = do
  (n, h) <- pOne ((,) <$> (length <$> some (eat "#"))
                      <*> untilp (eat "#" <|> eoi) (ch (const True)) <* roi)
  if n <= 6 then return (D ("h" ++ show n, []) [P [h]]) else empty

parag :: Parse String PorD
parag = D ("p", []) . return . P <$> roi

refdef :: Parse Char (String, [String])
refdef = (,) <$ punc "[" <*> some (ch isAlphaNum) <* punc "]:" <*> refStuff

refdefs :: Parse String PorD
refdefs = do
  s <- unlines <$> roi
  case parse (some refdef) s of
    Nothing -> empty
    Just (rds, "") -> return $
      D ("refdefs", [])
        [D ("ref", [("id", i)])
          [D ("rattr", [("value", s)]) [] | s <- ss]
        | (i, ss) <- rds]

parad :: Parse String PorD
parad = h1under <|> h2under <|> hhash <|> ruley <|> listy <|> refdefs <|> parag

grok :: PorD -> PorD
grok p@(P ss) = case parse (parad <* eoi) ss of
  Just (D tas ps, _) -> D tas ps  -- it's for parad to hit substructures
  _ -> p
grok p@(K ls) = p
grok (D tas ps) = D tas (map grok ps)

type RefDef = (String, [String])

chunkPorD :: [RefDef] -> PorD -> ([Chunk], [RefDef])
chunkPorD rs (P ss)      = case parse (many (chunk rs)) (intercalate "\n" ss) of
  Just (cs, "")  -> (cs, [])
  _              -> ([C 'e', C 'e', C 'k', C '!'], [])
chunkPorD _ (D ("refdefs", _) rs) = ([], rs >>= extract) where
  extract (D ("ref", as) ps) = case lookup "id" as of
    Just r -> [(r, ps >>= extrv)]
    _ -> []
  extract _ = []
  extrv (D ("rattr", as) _) = case lookup "value" as of
    Just s -> [s]
    Nothing -> []
chunkPorD rs (D tas ps)  = ([T tas cs, C '\n'], zs) where
  (cs, zs) = foldMap (chunkPorD rs) ps
chunkPorD ra (K ls)
  = ([T ("pre", []) [T ("code", []) (map C (unlines ls))]], [])

oxford :: [PorD] -> [Chunk]
oxford ps = cs where (cs, rs) = foldMap (chunkPorD rs) ps

chout :: Chunk -> String -> String
chout (C c) s = c : s
chout (E e) s = '&' : e ++ ';' : s
chout (T (t, as) cs) s = '<' : t ++ foldr aout rest as where
  rest = if null cs then (' ' : '/' : '>' : s) else
    '>' : foldr chout last cs
  last = '<' : '/' : t ++ '>' : s

aout :: (String, String) -> String -> String
aout (a, v) s = ' ' : a ++ '=' : '"' : v ++ '"' : s

title :: [Chunk] -> [Chunk] -> [Chunk]
title []                    = id
title (T ("h1", _) cs : _)  = pure cs
title (T _ cs : cs')        = title cs . title cs'
title (_ : cs)              = title cs

page :: [Chunk] -> Chunk
page cs =
  T ("html", [])
  [  T ("head", []) [T ("title", []) (title cs [])]
  ,  T ("body", []) cs
  ]

markdown :: String -> String
markdown = (`chout` []) . page . oxford . map grok . paras

main :: IO ()
main = interact markdown
