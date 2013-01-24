module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, when)
import Data.Maybe (isJust, fromJust, maybe)
import Data.Monoid ((<>))

import qualified Options.Applicative as O
import qualified Sound.TagLib as T

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
main = do
  opt <- O.execParser $ O.info (O.helper <*> parseArgs) $ O.fullDesc
           <> O.header "tag v0.1 - Command line editing of audio file tags"
  run opt


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
  mfile <- T.open fname
  mtag  <- maybe (return Nothing) T.tag mfile
  case mtag of
    Nothing  -> if optVerbose args then putStrLn ("missing tag: " ++ fname)
                                   else return ()
    Just tag -> handleTagFile (fromJust mfile) tag args

handleTagFile :: T.TagFile -> T.Tag -> Args -> IO ()
handleTagFile file tag args = do
  when (optVerbose args || not (anySetters args)) $ parseInfo tag >>= print
  when (anySetters args) $ do
    modifyTag tag args
    T.save file
    return ()

modifyTag :: T.Tag -> Args -> IO ()
modifyTag tag args = do
  withJust (optAlbum   args) (T.setAlbum   tag)
  withJust (optArtist  args) (T.setArtist  tag)
  withJust (optComment args) (T.setComment tag)
  withJust (optGenre   args) (T.setGenre   tag)
  withJust (optTitle   args) (T.setTitle   tag)
  withJust (optTrack   args) (T.setTrack   tag)
  withJust (optYear    args) (T.setYear    tag)
  where
    withJust m a = maybe (return ()) a m

anySetters :: Args -> Bool
anySetters args = or $ (sopt ++ iopt) <*> [args]
  where
    sopt = map (isJust .) [optAlbum, optArtist, optComment, optGenre, optTitle]
    iopt = map (isJust .) [optTrack, optYear]

parseInfo :: T.Tag -> IO Info
parseInfo tag = Info
  <$> T.album tag
  <*> T.artist tag
  <*> T.comment tag
  <*> T.genre tag
  <*> T.title tag
  <*> T.track tag
  <*> T.year tag

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
