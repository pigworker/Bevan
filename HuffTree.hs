module HuffTree where

import Data.Char
import Control.Applicative
import Data.List
import Data.Foldable hiding (concat, any, elem, sum)
import Data.Monoid

import ExParse
import Sig
import QuizUtils

data HuffComp
  = HLeaf Char (Maybe ([Bit], Maybe Int))
  | HNode (Bit, HuffTree) (Bit, HuffTree)
  deriving Show

type HuffTree = (Int, HuffComp)

type Bit = Int

pBit :: ExParse Char Bit
pBit = "a bit" $? (0 <$ toks "0" <|> 1 <$ toks "1")

pHuffTree :: ExParse Char HuffTree
pHuffTree = "a Huffman coding tree" $? ((,) <$> pInt <* spc <*> pHuffComp)

paren :: ExParse Char x -> ExParse Char x
paren p = id <$ toks "(" <* spc <*> p <* spc <* toks ")"

pHuffComp :: ExParse Char HuffComp
pHuffComp
  =    ("a letter" $? (HLeaf
         <$> eat isUpper
         <*> opt ((,)
              <$> ("a code" $? (id <$ spc <* toks "=" <* spc <*> some pBit))
              <*> opt ("a contribution" $? (id <$ spc <*> paren pInt)))))
  <|>  ("a fork" $? (HNode <$> pHuffBranch <* spc <*> pHuffBranch))

pHuffBranch :: ExParse Char (Bit, HuffTree)
pHuffBranch
  =    id <$ eat (`elem` "-\\|") <* spc <*> pHuffBranch
  <|>  (,) <$> ("a bit arrowhead" $? (id <$ toks "[" <*> pBit <* toks">"))
           <* spc
           <*> pHuffTree

huff :: String -> ([(Int, Char)], [Int])
huff prob = (freqs, gloms (map fst freqs))  where
  freqs = sort [(length x, head x) | x <- group (sort prob)]
  gloms :: [Int] -> [Int]
  gloms (a : b : cs) = ab : gloms (insert ab cs) where ab = a + b
  gloms cs = []

grokHuffTrees :: [HuffTree] -> (([(Int, Char)], [Int]), [String])
grokHuffTrees hts = ((sort fs, sort gs), es) where
  ((fs, gs), es) = foldMap (hTreeLeavesNodes []) hts
  hTreeLeavesNodes :: [Bit] -> HuffTree -> (([(Int, Char)], [Int]), [String])
  hTreeLeavesNodes bs (i, HLeaf c Nothing) = (([(i, c)], []), [])
  hTreeLeavesNodes bs (i, HLeaf c (Just (es, _))) =
    (([(i, c)], []), if bs == es then [] else [c : " is incorrectly coded.<br/>"])
  hTreeLeavesNodes bs (i, HNode (a, l@(j, _)) (b, r@(k, _))) =
    mconcat
      [  (([], [i]),
            (if i == j + k then [] else [concat
                [show i, " is not ", show j, " + ", show k, "<br/>"]]) ++
            (if sort [a,b] == [0,1] then []
                else ["Each fork should be labelled with distinct bits.<br/>"]))
      ,  hTreeLeavesNodes (bs ++ [a]) l
      ,  hTreeLeavesNodes (bs ++ [b]) r
      ]

freqErrors :: [(Int, Char)] -> [(Int, Char)] -> [String]
freqErrors probfs userfs
  = foldMap chkFreq probfs ++ foldMap freqChk (nub (map snd userfs)) where
    chkFreq (i, c)  = case [j | (j, d) <- userfs, c == d] of
      [] -> [c : " is missing from your frequency table.<br/>"]
      [j] -> case compare j i of
        GT -> [c : " is overcounted in your frequency table.<br/>"]
        EQ -> []
        LT -> [c : " is undercounted in your frequency table.<br/>"]
      _ -> [c : " occurs more than once in your frequency table.<br/>"]
    freqChk c = if any ((c ==) . snd) probfs then [] else
      [c : " is in your frequency table but not in the message.<br/>"]

groupError :: [Int] -> [Int] -> [String]
groupError [] [] = []
groupError (p : ps) [] = ["The tree grouping is fine, so far, but unfinished.<br/>"]
groupError (p : ps) (u : us) = case compare p u of
  EQ -> groupError ps us
  LT -> [concat ["You made a group of frequency ", show u, " when you could "
           , "have made a group of frequency ", show p, ".<br/>"]]
  GT -> [concat ["How did you make a group of frequency ", show u, " when "
           , "only frequency at least ", show p, "is possible?<br/>"]]
groupError [] _ = ["Your tree is somehow too big.<br/>"]

treeCodes :: HuffTree -> ([Int], [String])
treeCodes (_, HLeaf c Nothing) = ([], [c : " is missing its code.<br/>"])
treeCodes (_, HLeaf c (Just (bs, Nothing))) =
  ([], [c : " is missing its contribution.<br/>"])
treeCodes (n, HLeaf c (Just (bs, Just k)))
  | k == n * length bs = ([k], [])
  | otherwise = ([], [c : "'s contribution is incorrectly calculated.<br/>"])
treeCodes (_, HNode (_, l) (_, r)) = mappend (treeCodes l) (treeCodes r)

huffText :: String -> String -> Mo (String, Maybe [Int])
huffText prob attempt = do
  mhts <- syntaxCheck (some (spc *> pHuffTree)) attempt
  case mhts of
    Left e -> return (e, Nothing)
    Right hts -> do
      let (probfs, probgs) = huff prob
      let ((userfs, usergs), tes) = grokHuffTrees hts
      case tes ++ freqErrors probfs userfs of
        es@(_ : _) -> (,) <$> foldMap damn es <*> pure Nothing
        [] -> do
          case groupError probgs usergs of
            es@(_ : _) -> (,) <$> foldMap damn es <*> pure Nothing
            [] -> case hts of
              [t] -> case treeCodes t of
                (_, es@(_ : _)) -> (,) <$> foldMap damn es <*> pure Nothing
                (ks, _) ->
                   (,)  <$> praise "Good tree. Good codes.<br/>"
                        <*> pure (Just ks)
              _ -> (,)  <$> damn "Tell Conor something weird happened.<br/>"
                        <*> pure Nothing

huffProb :: String -> Mo String
huffProb prob = do
  (user, uh) <- textArea ("huffText" ++ prob) 15 80 ""
  (td, mcs) <- huffText prob user
  (_, mh, md) <- numBox ("huffLen" ++ prob) (sum <$> mcs)
  boxForm prob "Huffman Coding Problem" $ concat
    [  tag "code" "" prob
    ,  "<p/>"
    ,  uh
    ,  "<p/>"
    ,  td
    ,  "<p/>"
    ,  "Message length: ", mh, md
    ,  "<p/>"
    ]


------------------------ stuff for assessing Huffman tests

myHuff ::  [(String, String, String)]
myHuff = [("4","P","00")
         ,("3","A","100")
         ,("3","M","101")
         ,("3","T","110")
         ,("2","R","1110")
         ,("1","N","1111")
         ,("1","U","0100")
         ,("1","H","0101")
         ,("1","L","0110")
         ,("1","E","0111")
         ,("","","")
         ,("","","")
         ]

huffPre :: [(String, String, String)] -> [(Char, Int, [Bit])]
huffPre rs = sort
  [ (toUpper c, i, map (\ c -> ord c - ord '0') ys)
  | (fr, [c], xs) <- rs
  , isAlpha c
  , let ds = filter (not. isSpace) fr
  , all isDigit ds
  , let i = read ('0' : ds)
  , let ys = filter (not . isSpace) xs
  , all (`elem` "01") ys
  ]

huffFreqs :: String -> [(Char, Int)]
huffFreqs s = [(head w, length w) | w <- group (sort s)]

huffGroups :: [(Char, Int)] -> [Int]
huffGroups cis = glom (sort (map snd cis)) where
  glom (a : b : cs) = ab : glom (sort (ab : cs)) where ab = a + b
  glom cs = []
  

freqMoan :: [(Char, Int)] -> [(Char, Int, [Bit])] -> [String]
freqMoan cis djbss = go cis djbss where
  go [] [] = []
  go [] djbss = ["Spurious frequency for " ++ [d] ++ "!" | (d , _ , _) <- djbss]
  go cis [] = ["Missing frequency for " ++ [c] ++ "!" | (c, _) <- cis]
  go ((c, i) : cis') ((d, j, _) : djbss') =
    case (compare c d, compare i j) of
      (EQ, EQ) -> freqMoan cis' djbss'
      (EQ, _)  -> ["Miscounted " ++ [c] ++ "!"] ++ freqMoan cis' djbss'
      (LT, _)  -> ["Missing frequency for " ++ [c] ++ "!"] ++ freqMoan cis' djbss
      (GT, _)  -> case lookup d cis of
        Nothing -> ["Spurious frequency for " ++ [d] ++ "!"] ++ freqMoan cis djbss'
        Just _  -> ["Duplicate frequency for " ++ [d] ++ "!"] ++ freqMoan cis djbss'

codes2Tree :: [(Char, Int, [Bit])] -> Either [String] HuffTree
codes2Tree cibs = go [(c, i, bs, bs) | (c, i, bs) <- cibs] where
  go x | 1 < length [() | (_,_,[],_) <- x] = Left ["Coding not prefix free!"]
  go [(c, i, [], z)] = Right (i, HLeaf c (Just (z, Just (i * length z))))
  go [(c, i, _, _)] =
    Left ["Code for " ++ [c] ++ " does not come from a proper coding tree!"]
  go [] = Left ["There's an empty subtree in this tree!"]
  go xs = case
    ( go [(c, i, bs, z) | (c, i, 0 : bs, z) <- xs]
    , go [(c, i, bs, z) | (c, i, 1 : bs, z) <- xs]
    ) of
    (Right l@(y, _), Right r@(z, _)) -> Right (y + z, HNode (0, l) (1, r))
    (Left xs, Left ys) -> Left (xs ++ ys)
    (Left xs, _)       -> Left xs
    (_, Left ys)       -> Left ys

type Box = ((Int, Int), [String])

sBox :: String -> Box
sBox s = ((length s, 1), [s])

vpad :: Int -> Box -> Box
vpad h' ((w, h), xs) | h < h' =
  ((w, h'), xs ++ replicate (h' - h) (replicate w ' '))
vpad _ b = b

hpad :: Int -> Box -> Box
hpad w' ((w, h), xs) | w < w' =
  ((w', h), map (++ replicate (w' - w) ' ') xs)
hpad _ b = b

hjBox :: Box -> Box -> Box
hjBox b0@((w0, h0), xs0) b1@((w1, h1), xs1) =
  ((w0 + w1, max h0 h1),
    zipWith (++) (snd (vpad h1 b0)) (snd (vpad h0 b1)))

vjBox :: Box -> Box -> Box
vjBox b0@((w0, h0), xs0) b1@((w1, h1), xs1) =
  ((max w0 w1, h0 + h1),
    snd (hpad w1 b0) ++ snd (hpad w0 b1))

tsub :: Int -> Int -> Int
tsub m n | m > n = m - n
tsub _ _ = 0

tree2Box :: HuffTree -> Box
tree2Box (i, HLeaf c Nothing) = sBox (show i ++ " " ++ [c])
tree2Box (i, HLeaf c (Just (z, Nothing))) =
  sBox (show i ++ " " ++ [c,' '] ++ (z >>= show))
tree2Box (i, HLeaf c (Just (z, Just n))) =
  sBox (show i ++ " " ++ [c,' '] ++ (z >>= show) ++ " (" ++ show n ++ ")")
tree2Box (n, HNode (b, l) (d, r)) = case (tree2Box l, tree2Box r) of
  (b0@((w0, h0), xs0), b1@((w1, h1), xs1)) ->
    hjBox (sBox (show n ++ " ")) $ 
    hjBox lnk
      (vjBox
        (hjBox
          (sBox (replicate (1 + tsub w1 w0) '-' ++ "[" ++ show b ++ "> "))
          b0)
        (hjBox
          (sBox (replicate (1 + tsub w0 w1) '-' ++ "[" ++ show d ++ "> "))
          b1))
    where lnk = ((1, h0 + 1), ["-"] ++ replicate (h0 - 1) "|" ++ ["\\"])

nodeNums :: HuffTree -> [Int]
nodeNums (_, HLeaf _ _) = []
nodeNums (i, HNode (_, l) (_, r)) = i : nodeNums l ++ nodeNums r

tree2Seq :: HuffTree -> [Int]
tree2Seq x = sort (go x) where
  go (i, HNode (_, l) (_, r)) = i : go l ++ go r
  go _ = []

mkTable :: Int -> Mo [(String, String, String)]
mkTable i = do
  let fi = "f" ++ show i
  let li = "l" ++ show i
  let ci = "c" ++ show i
  f <- mo (VarDef fi)
  l <- mo (VarDef li)
  c <- mo (VarDef ci)
  if f && l && c
    then
      (:)
        <$> ((,,) <$> mo (Var fi) <*> mo (Var li) <*> mo (Var ci))
        <*> mkTable (i + 1)
    else return []

huffSpesh :: String -> Mo String
huffSpesh msg = do
  trs <- mkTable 0
  let hp = huffPre trs
  let hf = huffFreqs msg
  let hg = huffGroups hf
  let hfm = freqMoan hf hp
  case codes2Tree hp of
    Left es -> do
      return . unlines $
        ["<h3>Frequency</h3>"] ++ hfm ++
        ["<h3>Reconstituted Tree Error</h3>","<pre>"] ++ es
        ++ ["</pre>","<h3>Group Size Sequences</h3>", "from message: " ++ show hg ,
            "<h3>Message Length</h3>" ++ "correct message length: " ++ show (sum hg)]
    Right ht -> do
      let bs = snd (tree2Box ht)
      let hs = tree2Seq ht
      return . unlines $
        ["<h3>Frequency</h3>"] ++ hfm ++
        ["<h3>Reconstituted Tree</h3>","<pre>"] ++ bs
        ++ ["</pre>","<h3>Group Size Sequences</h3>",
        "from you: " ++ show hs, "from message: " ++ show hg]
