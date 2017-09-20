> {-# LANGUAGE ExistentialQuantification #-}

> module BinArith where

> import Control.Monad
> import Control.Applicative
> import Data.Traversable
> import Data.Foldable hiding (elem, all, foldl, concat, sum)
> import Data.Maybe
> import Data.Monoid
> import Data.Char
> import Data.List
> -- import Network.CGI
> import Text.XHtml
> import Numeric

> import QuizUtils
> import Sig
> import HackOldQuiz

> binArithPartA :: Mo String
> binArithPartA = boxForm "parta" "A Used Car Salesperson's Trick" =<< do
>   a <- mo $ Rnd (7000,9999)
>   b <- mo $ Rnd (1000, a - 1000)
>   (sn, sh, sm) <- numBox "pas" (Just (10000 - b))
>   (fn, fh, fm) <- numBox "paf" (Just (9999 - b))
>   chk <- case sn of
>     Nothing -> return << paragraph << italics << "This bit will appear later."
>     Just s -> do
>       (_, ch, cm) <- numBox "pac" (Just (a + s - 10000))
>       return << paragraph <<
>         [  id << "Check! Calculate "
>         ,  table <<
>            [  tr << [td noHtml, td << strong << show a, td << noHtml]
>            ,  tr << [td ! [align "right"] << strong << "+",
>                          td << strong << show s]
>            ,  tr << [td << strong << "= 1", td (primHtml ch), td (primHtml cm)]
>            ]
>         ]
>   return . renderHtml <<
>     [  paragraph <<
>        [  id << "I'm going to sell a car whose milometer currently reads "
>        ,  strong << show a, id << " but I know I can make more money if "
>        ,  id << " the car looks less heavily used. I would like to wind the "
>        ,  id << " meter back by ", strong << show b, id << " miles. "
>        ,  id << "Unfortunately, I can only wind the meter forwards. "
>        ,  id << "However, if I wind the meter past 9999, it goes back to "
>        ,  id << "0000."
>        ]
>     ,  paragraph <<
>        [  id << "Flip digits! Calculate "
>        ,  table <<
>           [  tr << [td noHtml, td << strong << "9999", td << noHtml]
>           ,  tr << [td << strong << "-", td << strong << show b]
>           ,  tr << [td << strong << "=", td (primHtml fh), td (primHtml fm)]
>           ]
>        ]
>     ,  paragraph <<
>        [  id << "How many miles must I wind the meter forwards to "
>        ,  id << "achieve the subtraction of ", strong << show b, id << "? "
>        ,  primHtml "&nbsp; &nbsp; "
>        ,  primHtml sh, primHtml sm
>        ]
>     ,  chk
>     ]

> adcp :: String -> Int -> Int -> Int -> Mo Html
> adcp k n x y = do
>     (co, [xh, yh, ch, zh]) <- adcr n x y (Just 0)
>     (_, xdh, xdm) <- numBox (k ++ "xd") (Just x)
>     (_, ydh, ydm) <- numBox (k ++ "yd") (Just y)
>     (_, zdh, zdm) <- numBox (k ++ "zd") (co >> Just (x + y))
>     return << table <<
>       [  tr << [  td noHtml, xh,bar, td << " in decimal ",
>                   td (primHtml xdh), td (primHtml xdm)]
>       ,  tr << [  td ! [align "right"] << strong << "+", yh, bar,
>                   td ! [align "right"] << strong << "+",
>                   td (primHtml ydh), td (primHtml ydm)]
>       ,  tr << [  td << strong << "carry", ch, td << strong << "0",
>                   td noHtml, td noHtml]
>       ,  tr << [  td << strong << "output", zh, bar,
>                   td ! [align "right"] << strong << "=",
>                   td (primHtml zdh), td (primHtml zdm)]
>       ]
>   where
>     bar = td ! [align "center"] << primHtml "&#8942;"
>     adcr :: Int -> Int -> Int -> Maybe Int -> Mo (Maybe Int, [Html])
>     adcr 0 x y c = return (c, [noHtml, noHtml, noHtml, noHtml])
>     adcr n x y c = do
>       let (x', x0) = divMod x 2
>       let (y', y0) = divMod y 2
>       let cz = divMod <$> (((x0 + y0) +) <$> c) <*> pure 2
>       (cn, ch, cm) <- numBox (k ++ show n ++ "c") (fst <$> cz)
>       (zn, zh, zm) <- numBox (k ++ show n ++ "z") (snd <$> cz)
>       (co, hs) <- adcr (n - 1) x' y' cn
>       return (co, zipWith (+++) hs
>         [  bar +++ (td ! [align "right"] << show x0) +++ td noHtml
>         ,  bar +++ (td ! [align "right"] << show y0) +++ td noHtml
>         ,  td (primHtml ch) +++ td (primHtml cm) +++ td noHtml
>         ,  bar +++ td (primHtml zh) +++ td (primHtml zm)
>         ])

> binArithPartB :: Mo String
> binArithPartB = boxForm "partb" "Unsigned Binary Addition" =<< do
>   x <- mo $ Rnd (64, 255)
>   y <- mo $ Rnd (64, 127)
>   bh1 <- adcp "b1k" 8 x y
>   x <- mo $ Rnd (64, 255)
>   y <- mo $ Rnd (64, 255)
>   bh2 <- adcp "b2k" 8 x y
>   x <- mo $ Rnd (64, 255)
>   bh3 <- adcp "b3k" 8 x x
>   return . renderHtml <<
>     [  paragraph <<
>          [  "In each column, add the two input bits to the carry-in "
>          ,  "from the right. Split your answer into the output bit "
>          ,  "and the carry-out to the left. You have three sums to do. "
>          ,  "There's something a wee bit special about the third. "
>          ,  "Check your answers by doing the same sums in decimal. "
>          ,  "Remember that the final carry-out contributes to the total!"
>          ]
>     ,  paragraph bh1
>     ,  paragraph bh2
>     ,  paragraph bh3
>     ]

> negp :: String -> Int -> Int -> Mo Html
> negp k n x = do
>     (co, [xh, yh, zh]) <- negr n x (Just 1)
>     return << table <<
>       [  tr << [td ! [align "right"]
>                << strong << [id << show x, sub << "dec", id << " to binary"],
>                  xh, bar]
>       ,  tr << [td ! [align "right"] << strong << "flip bits", yh, bar]
>       ,  tr << [td ! [align "right"] << strong << "add 1", zh, bar]
>       ]
>   where
>     bar = td ! [align "center"] << primHtml "&#8942;"
>     negr :: Int -> Int -> Maybe Int -> Mo (Maybe Int, [Html])
>     negr 0 x c = return (c, [noHtml, noHtml, noHtml])
>     negr n x c = do
>       let (x', x0) = divMod x 2
>       (xn, xh, xm) <- numBox (k ++ show n ++ "x") (Just x0)
>       (yn, yh, ym) <- numBox (k ++ show n ++ "y") ((1 -) <$> xn)
>       let cz = divMod <$> ((+) <$> yn <*> c) <*> pure 2
>       (zn, zh, zm) <- numBox (k ++ show n ++ "z") (snd <$> cz)
>       (co, hs) <- negr (n - 1) x' (fst <$> cz)
>       return (co, zipWith (+++) hs
>         [  bar +++ td (primHtml xh) +++ td (primHtml xm)
>         ,  bar +++ td (primHtml yh) +++ td (primHtml ym)
>         ,  bar +++ td (primHtml zh) +++ td (primHtml zm)
>         ])

> binArithPartC :: Mo String
> binArithPartC = boxForm "partc" "Two's Complement Negation" =<< do
>   ch1 <- negp "c1k" 8 =<< mo (Rnd (15, 63))
>   ch2 <- negp "c2k" 8 =<< mo (Rnd (64, 127))
>   ch3 <- negp "c3k" 8 1
>   ch4 <- negp "c4k" 8 128
>   return . renderHtml <<
>     [  paragraph <<
>        [  "Convert the given decimal numbers to binary, then compute "
>        ,  "their two's complement negations by flipping all the bits "
>        ,  "and adding 1 to the resulting binary number."
>        ]
>     ,  paragraph ch1
>     ,  paragraph ch2
>     ,  paragraph ch3
>     ,  paragraph ch4
>     ]

> subp :: String -> Int -> Int -> Int -> Mo Html
> subp k n x y = do
>     (co, [yh, fh, xh, ch, zh]) <- subr n x y (Just 1)
>     (_, xdh, xdm) <- numBox (k ++ "xd") (Just x)
>     (_, ydh, ydm) <- numBox (k ++ "yd") (Just y)
>     (_, zdh, zdm) <- numBox (k ++ "zd") (co >> Just (x - y))
>     return << table <<
>       [  tr << [  td noHtml, yh,bar,
>                   td << [italics << "y", id << " in decimal "],
>                   td (primHtml ydh), td (primHtml ydm)]
>       ,  tr << [td ! [align "right"] << strong << "flip", 
>                  fh, bar ]
>       ,  tr << [  td ! [align "right"] << strong << "+", xh, bar,
>                   td << [italics << "x", id << " in decimal "],
>                   td (primHtml xdh), td (primHtml xdm)]
>       ,  tr << [td << strong << "carry", ch, td << strong << "1"]
>       ,  tr << [td << strong << "output", zh, bar,
>                   td ! [align "right"] <<
>                         [italics << "x - y", strong << " = "],
>                   td (primHtml zdh), td (primHtml zdm)]
>       ]
>   where
>     bar = td ! [align "center"] << primHtml "&#8942;"
>     subr :: Int -> Int -> Int -> Maybe Int -> Mo (Maybe Int, [Html])
>     subr 0 x y c = return (c, [noHtml, noHtml, noHtml, noHtml, noHtml])
>     subr n x y c = do
>       let (x', x0) = divMod x 2
>       let (y', y0) = divMod y 2
>       (fn, fh, fm) <- numBox (k ++ show n ++ "f") (Just (1 - y0))
>       let cz = divMod <$> ((x0  +) <$> ((+) <$> fn <*> c)) <*> pure 2
>       (cn, ch, cm) <- numBox (k ++ show n ++ "c") (fst <$> cz)
>       (zn, zh, zm) <- numBox (k ++ show n ++ "z") (snd <$> cz)
>       (co, hs) <- subr (n - 1) x' y' cn
>       return (co, zipWith (+++) hs
>         [  bar +++ (td ! [align "right"] << show y0) +++ td noHtml
>         ,  bar +++ td (primHtml fh) +++ td (primHtml fm)
>         ,  bar +++ (td ! [align "right"] << show x0) +++ td noHtml
>         ,  td (primHtml ch) +++ td (primHtml cm) +++ td noHtml
>         ,  bar +++ td (primHtml zh) +++ td (primHtml zm)
>         ])

> binArithPartD :: Mo String
> binArithPartD = boxForm "partd" "Two's Complement Subtraction" =<< do
>   dh <- join $ subp "dk" 8 <$> mo (Rnd (64, 127)) <*> mo (Rnd (15, 127))
>   return .renderHtml <<
>     [  paragraph <<
>          [  id << "Subtraction combines negation and addition: to subtract a "
>          ,  id << "number, add its negation! Here, flip the bits of "
>          ,  italics << "y", id << ", the first number given, then add "
>          ,  italics << "x", id << ", the second number, to the result. "
>          ,  id << "Kicking off the adding carry with 1 does the 'add 1' "
>          ,  id << "part of negating ", italics << "y", id <<". You should "
>          ,  id << "find you have computed ", italics << "x - y"
>          ,  id << "! "
>          ,  id << "Check your work by converting to decimal."
>          ]
>     ,  paragraph dh
>     ]
