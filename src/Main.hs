module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM, (>=>))
import Data.Maybe (maybe)
import System.Environment (getArgs)
import qualified Sound.TagLib as TagLib

data Info = Info {
    album   :: String
  , artist  :: String
  , comment :: String
  , genre   :: String
  , title   :: String
  , track   :: Integer
  , year    :: Integer
  } deriving (Show)

main = do
  args <- getArgs
  mapM (readInfo >=> print) args

readInfo :: String -> IO (Maybe Info)
readInfo fname = do
  tagFile <- TagLib.open fname
  tag     <- onJust TagLib.tag tagFile
  onJust (parseInfo >=> return . Just) tag
-- readInfo fname =
--   TagLib.open fname >>= onJust TagLib.tag
--   >>= onJust (parseInfo >=> return . Just)

parseInfo :: TagLib.Tag -> IO Info
parseInfo tag = Info
  <$> TagLib.album tag
  <*> TagLib.artist tag
  <*> TagLib.comment tag
  <*> TagLib.genre tag
  <*> TagLib.title tag
  <*> TagLib.track tag
  <*> TagLib.year tag

onJust :: Monad m => (a -> m (Maybe b)) -> Maybe a -> m (Maybe b)
onJust f a = maybe (return Nothing) f a
