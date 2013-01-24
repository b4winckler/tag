module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, when)
import Data.Maybe (isJust, fromJust, maybe)
import Data.Monoid ((<>))

import qualified Options.Applicative as O
import qualified Sound.TagLib as TagLib

data Args = Args {
    optAlbum   :: Maybe String
  , optArtist  :: Maybe String
  , optComment :: Maybe String
  , optGenre   :: Maybe String
  , optTitle   :: Maybe String
  , optTrack   :: Maybe Integer
  , optYear    :: Maybe Integer
  , optVerbose :: Bool
  , optPaths   :: [FilePath]
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


main :: IO ()
main = O.execParser opts >>= run
  where
    opts = O.info (O.helper <*> parseArgs)
                  (O.fullDesc <> O.progDesc "Edit audio file tags")

parseArgs :: O.Parser Args
parseArgs = Args
  <$> parse O.str "album" "STR" 'a'
  <*> parse O.str "artist" "STR" 'b'
  <*> parse O.str "comment" "STR" 'c'
  <*> parse O.str "genre" "STR" 'g'
  <*> parse O.str "title" "STR" 't'
  <*> parse O.auto "track" "NUM" 'n'
  <*> parse O.auto "year" "NUM" 'y'
  <*> O.switch (O.long "verbose" <> O.help "Verbose output")
  <*> O.arguments1 O.str (O.metavar "FILES")
  where
    -- To parse a (Just a) value is complicated by the fact that we have to
    -- specify different readers for String and (Read a) values so we pass the
    -- reader as the first parameter.
    parse r name meta short = O.option $ O.reader (fmap Just . r)
                                      <> O.value Nothing
                                      <> O.long name
                                      <> O.short short
                                      <> O.metavar meta
                                      <> O.help ("Set " ++ name)

run :: Args -> IO ()
run args = forM_ (optPaths args) $ \fname -> do
  mfile <- TagLib.open fname
  mtag  <- maybe (return Nothing) TagLib.tag mfile
  case mtag of
    Nothing  -> putStrLn ("missing tag: " ++ fname) >> return ()
    Just tag -> handleTagFile (fromJust mfile) tag args

handleTagFile :: TagLib.TagFile -> TagLib.Tag -> Args -> IO ()
handleTagFile file tag args = do
  when (optVerbose args || not (anySetters args)) $ parseInfo tag >>= print
  when (anySetters args) $ do
    modifyTag tag args
    TagLib.save file
    return ()

modifyTag :: TagLib.Tag -> Args -> IO ()
modifyTag tag args = do
  withJust (optAlbum   args) (TagLib.setAlbum   tag)
  withJust (optArtist  args) (TagLib.setArtist  tag)
  withJust (optComment args) (TagLib.setComment tag)
  withJust (optGenre   args) (TagLib.setGenre   tag)
  withJust (optTitle   args) (TagLib.setTitle   tag)
  withJust (optTrack   args) (TagLib.setTrack   tag)
  withJust (optYear    args) (TagLib.setYear    tag)
  where
    withJust m a = maybe (return ()) a m

anySetters :: Args -> Bool
anySetters args = or $ (sopt ++ iopt) <*> [args]
  where
    sopt = map (isJust .) [optAlbum, optArtist, optComment, optGenre, optTitle]
    iopt = map (isJust .) [optTrack, optYear]

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
