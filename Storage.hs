module Storage where

import Prelude hiding (catch)
import System.IO
import System.IO.Error hiding (catch)
import Control.Exception(catch)
import System.FilePath
import System.Directory
import Control.Concurrent

import Data.Char
import Data.Time
-- import System.Locale
import Control.Applicative
import Text.ParserCombinators.ReadP
import Data.Foldable hiding (elem)

type UserName = String  -- must be valid username

data PageInfo = PageInfo
  {  pageName     :: [String]
  ,  pageUpdates  :: [PageUpdate]
  }

data PageUpdate = PageUpdate
  {  updateKey    :: String
  ,  updateTime   :: String
  ,  updateValue  :: String
  }

-- A copy of eof, for oldness' sake

ceof :: ReadP ()
-- ^ Succeeds iff we are at the end of input
ceof = do { s <- look 
          ; if null s then return () 
                      else pfail }

line :: String -> String
line s = s ++ "\n"

instance Show PageUpdate where
  show (PageUpdate { updateKey = k, updateTime = t, updateValue = v }) =
    k ++ " " ++ t ++ " " ++
    blat (lines v)

blat :: [String] -> String
blat [] = line ""
blat [c : cs] | elem c "| {"  = line $ '|' : c : cs
              | otherwise     = line $ c : cs
blat ls =
  line (replicate b '{') ++
  foldMap line ls ++
  line (replicate b '}')
 where
   b = 1 + grob 0 0 ls
   grob m c [] = max m c
   grob m c ("" : ls) = grob (max m c) 0 ls
   grob m c (('}' : cs) : ls) = grob m (c + 1) (cs : ls)
   grob m c ((_ : cs) : ls) = grob (max m c) 0 (cs : ls)

instance Show PageInfo where
  show (PageInfo {pageName = nom, pageUpdates = ups}) =
    line "================================================================="
    ++ line (unwords nom)
    ++ line "================================================================="
    ++ foldMap show ups

instance Read PageInfo where
  readsPrec _ = readP_to_S $ do
    skipSpaces
    munch1 (== '=')
    skipSpaces
    nom <- words <$> munch (\ c -> isAlphaNum c || elem c " -")
    skipSpaces
    munch1 (== '=')
    ups <- manyTill readPPageUpdate $ (skipSpaces >> (ceof <++ do
      '=' : _ <- look
      return ()))
    skipSpaces
    return (PageInfo {pageName = nom, pageUpdates = ups})

instance Read PageUpdate where
  readsPrec _ = readP_to_S readPPageUpdate

readPPageUpdate :: ReadP PageUpdate
readPPageUpdate = do
  skipSpaces
  k <- munch1 (\ c -> isAlphaNum c || elem c "._/")
  skipSpaces
  t <- munch1 (`elem` "0123456789-")
  s <- readBlat
  return (PageUpdate {updateKey = k, updateTime = t, updateValue = s})

readBlat :: ReadP String
readBlat = do
  munch (`elem` " \t")
  c <- get <++ (ceof >> return '\n')
  case c of
    '\n' -> return ""
    '|' -> munch (/= '\n')
    '{' -> do
      b <- munch (== '{')
      let c = 1 + length b
      munch (/= '\n')
      char '\n'
      manyTill (satisfy (const True)) (string (replicate c '}'))
    c -> do
      cs <- munch (/= '\n')
      return (c : cs)


unix :: String -> String
unix [] = []
unix ('\r' : '\n' : xs) = '\n' : unix xs
unix ('\r' : xs) = '\n' : unix xs
unix ('\n' : xs) = '\n' : unix xs
unix ('\t' : xs) = ' ': unix xs
unix (x : xs) | x < ' ' || ord x > 126 = unix xs
unix (x : xs) = x : unix xs

timeStamp :: IO String
timeStamp = (take 20 . formatTime defaultTimeLocale "%y-%m-%d-%H-%M-%S-%q")
          <$> getZonedTime

rpage :: [String] -> String -> PageInfo
rpage pa s = case reads s of
  [] -> PageInfo pa []
  ((x, s) : _) -> x

dopeOn  :: FilePath       -- root for private data store for class
        -> UserName       -- student/staff id
        -> [String]       -- path within class
        -> IO PageInfo
dopeOn sdir stu pa = do
  stuf <- tryRead 3 (sdir </> stu </> joinPath pa </> "info")
  True <- case length stuf of -- force lazy file io
    0 -> return True
    _ -> return True
  return (rpage pa stuf)

myReadFile :: FilePath -> IO String
myReadFile f = withFile f ReadMode $ \ h -> do
  hSetEncoding h utf8
  stuf <- hGetContents h
  True <- case length stuf of -- force lazy file io
    0 -> return True
    _ -> return True
  hClose h
  return stuf

tryRead :: Int -> FilePath -> IO String
tryRead 0 f = ioError (userError "read access timed out")
tryRead n f = catch (myReadFile f) $ \ e ->
    case 0 of
      _ | isDoesNotExistError e -> return ""
        | isAlreadyInUseError e -> threadDelay 1000000 >>
             tryRead (n - 1) f
        | otherwise -> ioError e 

dopeUp :: FilePath       -- root for private data store for class
        -> UserName       -- student/staff id
        -> [String]       -- path within class
        -> PageInfo -> IO ()
dopeUp sdir stu pa sin = do
  let s = show sin
  if length s > 0  -- don't open the file until you know you have the goods
    then do
      let apa = sdir </> stu </> joinPath pa
      createDirectoryIfMissing True apa
      let tf = apa </> "update"
      let nf = apa </> "info"
      catch (writeFile tf s) $ \ e ->
        if isDoesNotExistError e
          then do
            createDirectoryIfMissing True (sdir </> stu)
            writeFile (sdir </> tf) s
          else ioError e
      copyFile tf nf
    else return ()
