> {-# LANGUAGE DeriveTraversable, DeriveFoldable, DeriveFunctor, FlexibleContexts,
>     ConstraintKinds, GADTs, ScopedTypeVariables, RankNTypes #-}

> module CVI where

> import Data.List
> import Data.Maybe
> import Control.Monad
> import Control.Applicative
> import Data.Traversable
> import Data.Foldable
> import Data.Monoid
> import Control.Monad.State

> data Direction = Down | Up deriving (Show, Eq)

> class Invertible x where
>   invert :: x -> x
>   -- law   invert . invert = id
>   (~~) :: Direction -> x -> x
>   Down  ~~ x   = x
>   Up    ~~ x   = invert x

> instance Invertible Direction where
>   invert Down  = Up
>   invert Up    = Down

> data SignalType x
>   = Direction :~ x
>   | Bit Direction
>   | Cable [SignalType x]
>   | [SignalType x] :=> [SignalType x]
>   deriving (Show, Eq, Traversable, Foldable, Functor)

> instance Invertible (SignalType x) where
>   invert (d :~ x)     = invert d :~ x
>   invert (Bit d)      = Bit (invert d)
>   invert (Cable ts)   = Cable (map invert ts)
>   invert (ss :=> ts)  = map invert ts :=> map invert ss

> instance Monad SignalType where
>   return x = Down :~ x
>   (d :~ x)     >>= f  = d ~~ f x
>   Bit d        >>= f  = Bit d
>   Cable ts     >>= f  = Cable (map (>>= f) ts)
>   (ss :=> ts)  >>= f  = map (>>= f) ss :=> map (>>= f) ts

> data Var = Poly String | Unif Integer deriving Eq

> instance Show Var where
>   show (Poly x) = x
>   show (Unif i) = show i

> type Subst m = [(Integer, m Var)]
> subst :: Monad m => Subst m -> Var -> m Var
> subst its t@(Unif i)  = fromMaybe (return t) (lookup i its)
> subst its v           = return v

> data TError m
>   = Conflict (m Var, m Var)
>   | Cyclic (Integer, m Var)
>   | Stray Var

> data Dict c where
>   Dict :: c => Dict c

> dict :: Dict c -> (c => x) -> x
> dict Dict x = x

> class StarToStarShow f where
>   starToStarShow :: Show x => Dict (Show (f x))

> instance StarToStarShow SignalType where
>   starToStarShow = Dict

> instance StarToStarShow m => Show (TError m) where
>   show (Conflict p) = "Conflict " ++
>     dict (starToStarShow :: Dict (Show (m Var))) (show p)
>   show (Cyclic p) = "Cyclic " ++
>     dict (starToStarShow :: Dict (Show (m Var))) (show p)

> newtype Checking m x =
>   Checking {checking :: Integer -> Subst m ->
>                         Either (TError m) (x, Integer, Subst m)}

> terror :: TError m -> Checking m x
> terror e = Checking $ \ _ _ -> Left e

> defined :: Integer -> Checking m (Maybe (m Var))
> defined j = Checking $ \ n its -> Right (lookup j its, n, its)

> fresh :: Checking m Var
> fresh = Checking $ \ n its -> Right (Unif n, n + 1, its)

> instance Monad (Checking m) where
>   return x = Checking $ \ n its -> Right (x, n, its)
>   Checking f >>= k = Checking $ \ n its -> case f n its of
>     Left e -> Left e
>     Right (x, n, its) -> checking (k x) n its

> instance Applicative (Checking m) where
>   pure = return
>   (<*>) = ap  -- ought to Brucify this sometime

> instance Functor (Checking m) where
>   fmap = (<*>) . pure

> instance Monoid x => Monoid (Checking m x) where
>   mempty = pure mempty
>   mappend a b = mappend <$> a <*> b

> class (Monad m, Foldable m) => Unifiable m where
>   unify :: (m Var, m Var) -> Checking m ()

> flexRigid :: Unifiable m => Integer -> m Var -> Checking m ()
> flexRigid j t = do
>   mu <- defined j
>   case mu of
>     Just u -> unify (t, u)
>     Nothing -> do
>       foldMap (noccur j t) t
>       Checking $ \ n its -> Right ((), n, (j, t) : its)

> noccur :: (Foldable m, Monad m) => Integer -> m Var -> Var -> Checking m ()
> noccur _ _ (Poly _) = return ()
> noccur j t (Unif i) | i == j = terror (Cyclic (j, t))
> noccur j t (Unif i) = do
>   mu <- defined i
>   case mu of
>     Nothing -> return ()
>     Just u -> foldMap (noccur j (t >>= subst [(i, u)])) u

> instance Unifiable SignalType where
>   unify (d :~ u, d' :~ v) | d == d' && u == v = return ()
>   unify (d :~ Unif i, t) = flexRigid i (d ~~ t)
>   unify (t, d :~ Unif i) = flexRigid i (d ~~ t)
>   unify (Bit d, Bit d') | d == d' = return ()
>   unify (Cable ts, Cable us) | length ts == length us
>     = foldMap unify (zip ts us)
>   unify (ss :=> ts, us :=> vs)
>     | length ss == length us && length ts == length vs
>     = foldMap unify (zip ss us) >> foldMap unify (zip ts vs)
>   unify p = terror (Conflict p)

> type Scheme m = ([String], m Var)

> freshen :: (Traversable m, Monad m) => Scheme m -> Checking m (m Var)
> freshen (xs, t) = do
>   xis <- for xs $ \ x -> (,) x <$> fresh
>   return $ t >>= \ v -> case v of
>     Poly x -> case lookup x xis of
>       Just i -> return i
>       Nothing -> return (Poly x)
>     v -> return v

> type Wire = (Integer, Bool) -- name, negation status

> data Bit = B0 | B1 deriving (Show, Eq)

> data Signal
>   = W Direction Wire
>   | B Bit
>   | T [Signal]
>   deriving (Show, Eq)

> instance Invertible Signal where
>   invert (W d w)  = W (invert d) w
>   invert (B v)    = B v
>   invert (T xs)   = T (map invert xs)

> class Neggable x where
>   neg :: x -> x

> nact :: Neggable x => Bool -> x -> x
> nact True   = id
> nact False  = neg

> instance Neggable Bit where
>   neg B0 = B1
>   neg B1 = B0

> instance Neggable Signal where
>   neg (W d (i, b))  = W d (i, not b)
>   neg (B v)         = B (neg v)
>   neg (T xs)        = T (map neg xs)

> data Circuit = Circuit
>   {  nextWire    :: Integer
>   ,  wireJoins   :: [(Integer, Wire)]
>   ,  knownWires  :: [(Integer, Bit)]
>   ,  andBlocks   :: [(Integer, (Wire, Wire))]
>   }  deriving Show

> emptyCircuit = Circuit 0 [] [] []

> newtype Sem x = Sem {sem :: Circuit -> Maybe (x, Circuit)}

> getCircuit :: Sem Circuit
> getCircuit = Sem $ \ c -> Just (c, c)

> putCircuit :: Circuit -> Sem ()
> putCircuit c = Sem $ \ _ -> Just ((), c)

> abort :: Sem a
> abort = Sem $ \ _ -> Nothing

> instance Monad Sem where
>   return x = Sem $ \ c -> Just (x, c)
>   Sem f >>= k = Sem $ \ c -> do
>     (x, c) <- f c
>     sem (k x) c

> instance Applicative Sem where
>   pure = return
>   (<*>) = ap

> instance Functor Sem where
>   fmap = (<*>) . pure

> newWire :: Direction -> Sem Signal
> newWire d = do
>   c <- getCircuit
>   let i = nextWire c
>   putCircuit $ c {nextWire = i + 1}
>   return (W d (i, True))

> hnfWire :: Wire -> Sem Signal
> hnfWire (i, b) = nact b <$>do
>   c <- getCircuit
>   case lookup i (knownWires c) of
>     Just v   -> return (B v)
>     Nothing  -> case lookup i (wireJoins c) of
>       Just  w  -> hnfWire w
>       Nothing  -> return (W Down (i, True))

> andS :: Signal -> Signal -> Sem Signal  -- inputs are down-wires or bits
> andS (B B0)  _                     = return (B B0)
> andS _       (B B0)                = return (B B0)
> andS (B B1)  x                     = return x
> andS x       (B B1)                = return x
> andS x       y       | x == y      = return x
> andS x       y       | x == neg y  = return (B B0)
> andS (W Down w) (W Down u) = do
>   c <- getCircuit
>   let i = nextWire c
>   putCircuit $c {nextWire = i + 1, andBlocks = (i, (w, u)) : andBlocks c}
>   return (W Down (i, True))
> andS x y = error $ "andS " ++ show x ++ " " ++ show y

> update :: Signal -> Sem Signal
> update (B b)         = return (B b)
> update x@(W Up _)    = return x
> update x@(W Down w)  = hnfWire w
> update (T xs)        = T <$> traverse update xs

> wireVal :: Integer -> Bit -> Sem ()
> wireVal i v = do
>   c <- getCircuit
>   case [u | (j, u) <- knownWires c, i == j] of
>     u : _  | u == v -> return ()  -- nothing new
>            | otherwise -> abort   -- conflicting information
>     [] -> do
>       putCircuit $ c {knownWires = (i, v) : knownWires c}
>       c <- getCircuit
>       let (js, ks) = partition (\ (_, (k, _)) -> i == k) (wireJoins c)
>       putCircuit $ c {wireJoins = ks}
>       c <- getCircuit
>       for js $ \ (j, (_, b)) -> wireVal j (nact b v)
>       let (as, bs) = partition (\ (_, ((j, _), (k, _))) -> i == j || i == k)
>                        (andBlocks c)
>       putCircuit $ c {andBlocks = bs}
>       for as $ \ (j, (w, v)) -> do
>         x <- hnfWire w
>         y <- hnfWire v
>         z <- andS x y
>         wireSignal j z
>       return ()

> wireSignal :: Integer -> Signal -> Sem ()
> wireSignal i (B v) = wireVal i v
> wireSignal i (W Down (j, b))
>   | i == j = if b then return () else abort
>   | otherwise = do
>     c <- getCircuit
>     putCircuit $ c {wireJoins = (i, (j, b)) : wireJoins c}
>     c <- getCircuit
>     let (as, bs) = partition
>           (\ (_, ((l, _), (k, _))) -> sort [l, k] == sort [i, j])
>           (andBlocks c)
>     putCircuit $ c {andBlocks = bs}
>     for as $ \ (j, (w, v)) -> do
>       x <- hnfWire w
>       y <- hnfWire v
>       z <- andS x y
>       wireSignal j z
>     return ()

> data Tile
>   = Vert
>   | CapL | CapR 
>   | CupL | CupR
>   | Swap
>   | DupD | DupU
>   | AndD | AndU
>   | NotD | NotU
>   deriving Show

> instance Invertible Tile where -- reflects in horizontal axis
>   invert Vert = Vert
>   invert CapL = CupL
>   invert CapR = CupR
>   invert CupL = CapL
>   invert CupR = CapR
>   invert Swap = Swap
>   invert DupD = DupU
>   invert DupU = DupD
>   invert AndD = AndU
>   invert AndU = AndD
>   invert NotD = NotU
>   invert NotU = NotD


> tile :: Tile -> [Signal] -> Sem ([Signal], [Signal])
> tile Vert (x : xs) = return ([x], xs)
> tile CapL xs = newWire Down >>= \ x -> return ([x, invert x], xs)
> tile CapR xs = newWire Down >>= \ x -> return ([invert x, x], xs)
> tile CupL (W Up (i, b) : s : xs) = do
>   wireSignal i (nact b s)
>   (,) [] <$> traverse update xs
> tile CupR (s : W Up (i, b) : xs) = do
>   wireSignal i (nact b s)
>   (,) [] <$> traverse update xs
> tile Swap (x : y : zs) = return ([y, x], zs)
> tile DupD (x : xs) = return ([x, x], xs)
> tile DupU (W Up (i, b) : y : zs) = do
>   wireSignal i (nact b (invert y))
>   (,) <$> ((:[]) <$> update y) <*> traverse update zs
> tile AndD (x : y : zs) = do
>   w <- andS x y
>   return ([w], zs)
> tile AndU (W Up (i, b) : zs) = do
>   w <- newWire Up
>   v <- newWire Up
>   z <- andS (invert w) (invert v)
>   wireSignal i (nact b z)
>   zs <- traverse update zs
>   return ([w, v], zs)
> tile NotD (x : xs) = return ([neg x], xs)
> tile NotU (x : xs) = return ([neg x], xs)

> tiles :: [Tile] -> [Signal] -> Sem [Signal]
> tiles [] [] = return []
> tiles (t : ts) xs = do
>   (ys, xs) <- tile t xs
>   zs <- tiles ts xs
>   ys <- traverse update ys
>   return (ys ++ zs)

> circuit :: [[Tile]] -> [Signal] -> Sem [Signal]
> circuit [] xs = return xs
> circuit (ts : tss) xs = do
>   xs <- tiles ts xs
>   circuit tss xs

