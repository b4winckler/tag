module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, (>=>))
import Data.Monoid
import Data.Maybe (catMaybes, maybe, fromJust)
import System.Environment (getArgs)
import Options.Applicative

import qualified Sound.TagLib as TagLib

data Args = Args {
    optAlbum   :: Maybe String
  , optArtist  :: Maybe String
  , optComment :: Maybe String
  , optGenre   :: Maybe String
  , optTitle   :: Maybe String
  , optFiles   :: [String]
  }

data Info = Info {
    album   :: String
  , artist  :: String
  , comment :: String
  , genre   :: String
  , title   :: String
  , track   :: Integer
  , year    :: Integer
  }


main = execParser opts >>= run
  where
    opts = info (helper <*> parseArgs)
                (fullDesc <> progDesc "Edit audio file tags")

parseArgs :: Parser Args
parseArgs = Args
  <$> maybeStrOption (long "album"   <> metavar "ALBUM"   <> help "Set album")
  <*> maybeStrOption (long "artist"  <> metavar "ARTIST"  <> help "Set artist")
  <*> maybeStrOption (long "comment" <> metavar "COMMENT" <> help "Set comment")
  <*> maybeStrOption (long "genre"   <> metavar "GENRE"   <> help "Set genre")
  <*> maybeStrOption (long "title"   <> metavar "TITLE"   <> help "Set title")
  <*> arguments1 str (metavar "FILES")

maybeStrOption m = option (reader (return . Just) <> value Nothing <> m)

run :: Args -> IO ()
run args = forM_ (optFiles args) $ \fname -> do
  mfile <- TagLib.open fname
  mtag  <- maybe (putStrLn ("open failed: " ++ fname) >> return Nothing)
                 TagLib.tag mfile
  case mtag of
    Nothing  -> putStrLn ("missing tag: " ++ fname) >> return ()
    Just tag -> do
      maybe (return ()) (TagLib.setAlbum   tag) (optAlbum args)
      maybe (return ()) (TagLib.setArtist  tag) (optArtist args)
      maybe (return ()) (TagLib.setComment tag) (optComment args)
      maybe (return ()) (TagLib.setGenre   tag) (optGenre args)
      maybe (return ()) (TagLib.setTitle   tag) (optTitle args)

      if null $ catMaybes [optAlbum args, optArtist args, optComment args,
                           optGenre args, optTitle args]
        then parseInfo tag >>= print
        else TagLib.save (fromJust mfile) >> return ()

readInfo :: String -> IO (Maybe Info)
readInfo fname = do
  mfile <- TagLib.open fname
  mtag  <- maybe (return Nothing) TagLib.tag mfile
  maybe (return Nothing) (parseInfo >=> return . Just) mtag

parseInfo :: TagLib.Tag -> IO Info
parseInfo tag = Info
  <$> TagLib.album tag
  <*> TagLib.artist tag
  <*> TagLib.comment tag
  <*> TagLib.genre tag
  <*> TagLib.title tag
  <*> TagLib.track tag
  <*> TagLib.year tag

instance Show Info where
  show (Info alb art cmt gen tle trk yr) = concat [
      line "artist:  " art
    , line "album:   " alb
    , line "title:   " tle
    , line "track:   " $ show trk
    , line "year:    " $ show yr
    , line "genre:   " gen
    , line "comment: " cmt
    ]
    where
      line _ "" = ""
      line l s  = l ++ s ++ "\n"
