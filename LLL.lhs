> {-# LANGUAGE DeriveFunctor, DeriveFoldable, DeriveTraversable #-}

> module LLL where

> import Data.Foldable
> import Data.Traversable
> import Control.Applicative
> import Control.Monad

> data Exp x
>   = Exp x :$ [Exp x]
>   | Tup [Exp x]
>   | Splay (Exp x)
>   | Con  Bit
>   | In   Int
>   | Def  x
>   deriving (Show, Functor, Foldable, Traversable)

> instance Monad Exp where
>   return = Def
>   Def x >>= k = k x
>   (f :$ as) >>= k = (f >>= k) :$ map (>>= k) as
>   Tup es >>= k = Tup (map (>>= k) es)
>   Splay e >>= k = Splay (e >>= k)
>   Con b >>= k = Con b
>   In i >>= k = In i

> instance Applicative Exp where
>   pure = return
>   (<*>) = ap

> type Prog f x = ([([f], Exp x)],[Exp x])

> data Val
>   = VB [Bit]
>   | VT [Val]
>   | VF String ([Val] -> [Val])

> data Bit = BU | B0 | B1 deriving Eq
> instance Show Bit where
>   show BU = "#"
>   show B0 = "0"
>   show B1 = "1"

> instance Show Val where
>   show (VB bs) = bs >>= show
>   show (VT vs) = "(" ++ (vs >>= show) ++ ")"
>   show (VF s _) = s

> zipw :: (a -> b -> c) -> [a] -> [b] -> [c]
> zipw f [] _ = []
> zipw f (x : xs) ~(v : vs) = f x v : zipw f xs vs

> oxford :: (Show x, Eq x) => [(x, Val)] -> Prog x x -> [Val] ->[Val]
> oxford gevs (defs, es) ivs = es >>= eval where
>   levs = defs >>= \ (ns, e) -> zipw (,) ns (eval e)
>   evs = levs ++ gevs
>   -- eval :: Exp x -> [Val] -- grr
>   eval (In i)     = [ivs !! i]
>   eval (Def x)    = case lookup x evs of
>     Just v -> [v]
>     _ -> error $ "what's " ++ show x ++ "?"
>   eval (f :$ es)  = eval f $$ (es >>= eval)
>   eval (Tup es)   = [VT (es >>= eval)]
>   eval (Splay e)  = vs where [VT vs] = eval e
>   eval (Con b)    = [VB (repeat b)]
>   ($$) :: [Val] -> [Val] -> [Val]
>   ~[~(VF _ f)] $$ vs = f vs

> andG :: [Val] -> [Val]
> andG ~[~(VB xs), ~(VB ys)] = [VB (zipw (*) xs ys)] where

> notG :: [Val] -> [Val]
> notG ~[~(VB xs)] = [VB (map naw xs)] where
>   naw BU = BU
>   naw B0 = B1
>   naw B1 = B0

> xorG :: [Val] -> [Val]
> xorG ~[~(VB xs), ~(VB ys)] = [VB (zipw (+) xs ys)] where

> clkSRFF :: [Val] -> [Val]
> clkSRFF ~[~(VB cs), ~(VB ss), ~(VB rs)] = [VB (go B0 cs B0 (B0, B0) ss rs)]
>   where
>     go c cs q sr ss rs = q : mo c cs q sr ss rs
>     mo B0 (B1 : cs) q _ (s : ss) (r : rs) = go B1 cs q (s, r) ss rs
>     mo B1 (B0 : cs) q sr (_ : ss) (_ : rs) = go B0 cs q' sr ss rs where
>       q' = case (sr, q) of
>              ((B0, B0), q) -> q
>              ((B1, B0), q) -> B1
>              ((B0, B1), q) -> B0
>              ((B1, B1), q) -> BU
>     mo _ (c : cs) q sr (_ : ss) (_ : rs) = go c cs q sr ss rs
>     mo _ _ _ _ _ _ = []

> clock :: Val
> clock = VB (cycle [B1, B0])

> till :: Int -> Val
> till t = VB [b | i <- [0..t], b <- [B1, B0]]

> control :: [Bit] -> Val
> control bs = VB [b | b <- bs, _ <- [0,1]]

> instance Num Bit where
>   fromInteger 0 = B0
>   fromInteger 1 = B1
>   BU  + _   = BU -- xor
>   _   + BU  = BU
>   b   + b'  | b == b'    = B0
>             | otherwise  = B1
>   BU  * _   = BU -- and
>   _   * BU  = BU
>   B0  * b   = B0
>   B1  * b   = b
>   abs = undefined
>   signum = undefined
>   negate = undefined
