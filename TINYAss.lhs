> module TINYAss where

> import Data.Char
> import Data.Monoid
> import Data.Foldable (foldMap)
> import Data.Traversable
> import Control.Applicative
> import Control.Monad.Writer

> import TINY
> import ExParse

> type EP = ExParse Char



> type Program = [Chunk]
> program :: EP Program
> program = [] <$ spc <* eoi <|> (:) <$ spc <*> chunk <*> program

> data Chunk
>   = L Label
>   | I Op (Maybe Operand)
>   | D V
>   deriving Show
> type Label = String
> type Operand = (Label, Maybe V)

> badLabel :: String -> String
> badLabel l
>   | elem (map toUpper l) (map show [HLT .. FLA]) =
>       "something like an operator, " ++ l ++
>       ", but not all in capitals"
>   | otherwise = "a label definition, " ++ l ++ ", had no : affixed!"

> chunk :: EP Chunk
> chunk  = "a chunk" $? (
>             (label >>= \ l -> spc *>
>                   (L l <$ toks ":" <|>
>                    (badLabel l $? empty)))
>        <|>  (bigOp >>= \ o -> I o <$ spc <*>
>                 (Just <$> operand
>                  <|>
>                  (("an operator, " ++ show o ++ " with no operand") $? empty)))
>        <|>  I <$> weeOp <*> pure Nothing
>        <|>  D <$> hex
>        <|>  (eat (const True) >>= \ c -> ("an unexpected " ++ [c]) $? empty)
>        <|>  ("a mystery" $? empty))

> label :: EP Label
> label = (:) <$> eat isLower <*> many (eat isAlpha)

> bigOp :: EP Op
> bigOp  =    JMP <$ toks "JMP"
>        <|>  JZE <$ toks "JZE"
>        <|>  JNZ <$ toks "JNZ"
>        <|>  LDA <$ toks "LDA"
>        <|>  STA <$ toks "STA"
>        <|>  ADC <$ toks "ADC"
>        <|>  LDL <$ toks "LDL"

> weeOp :: EP Op
> weeOp  =    HLT <$ toks "HLT"
>        <|>  GET <$ toks "GET"
>        <|>  PUT <$ toks "PUT"
>        <|>  ROL <$ toks "ROL"
>        <|>  ROR <$ toks "ROR"
>        <|>  SCF <$ toks "SCF"
>        <|>  CCF <$ toks "CCF"
>        <|>  DEL <$ toks "DEL"
>        <|>  FLA <$ toks "FLA"

> hex :: EP V
> hex  = "a hex digit" $? (
>           read <$> ((:[]) <$> eat isDigit)
>      <|>  10 <$ toks "A"
>      <|>  11 <$ toks "B"
>      <|>  12 <$ toks "C"
>      <|>  13 <$ toks "D"
>      <|>  14 <$ toks "E"
>      <|>  15 <$ toks "F")

> operand :: EP Operand
> operand = "an operand" $? ((,) <$> label <*>
>    (Just <$ spc <* toks "+" <* spc <*> hex <|> pure Nothing))

> (+/) :: V -> V -> V
> i +/ j = mod (i + j) 16

 instance Error e => Applicative (Either e) where
   pure = return
   (<*>) = ap

> symTab :: V -> [Chunk] -> [(Label, V)]
> symTab i [] = []
> symTab i (L l : cs) = (l, i) : symTab i cs
> symTab i (I _ (Just _) : cs) = symTab (i +/ 2) cs
> symTab i (I _ Nothing : cs) = symTab (i +/ 1) cs
> symTab i (D _ : cs) = symTab (i +/ 1) cs

> assemble :: [(Label, V)] -> [Chunk] -> [Maybe V]
> assemble tab cs = foldMap (asm tab) cs where
>   asm _ (L _) = []
>   asm t (I o Nothing) = [Just (fromEnum o)]
>   asm t (I o (Just u)) = [Just (fromEnum o), ev t u]
>   asm t (D v) = [Just v]
>   ev tab (l, o) = case (lookup l tab, o) of
>     (Nothing, _) -> Nothing
>     (Just v, Nothing) -> Just v
>     (Just v, Just u) -> Just (v +/ u)
