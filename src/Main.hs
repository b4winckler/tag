--
-- Operation:
--  * When one or more filenames are specified the program will look for audio
--    tags in those files (and ignore files with no tag).  If an option to
--    modify a tag is used then the files will be modified.  Otherwise all
--    recognized tags are printed to stdout.
--  * When no filenames are specified TSV formatted input is read from stdin
--    (the first line must be a header).  The use case is to first read tags
--    from audio files and pipe the output to a filter (e.g. awk) and finally
--    to pipe this to the program which reads the TSV from stdin.
--  * The TSV header must follow the format specified in tsvHeader (this is not
--    a limitation of the program, but rather an attempt to standardize scripts
--    which use the program)
--
-- Limitations:
--  * TSV code only works for tags and filenames without tabs in them (the code
--    does nothing to deal with tabs)
--

{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, when)
import Data.Char (ord)
import Data.Csv
import Data.Maybe (isJust, fromJust, maybe)
import Data.Monoid ((<>))

import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Options.Applicative as O
import qualified Sound.TagLib as T

data Args = Args {
    optAlbum        :: Maybe String
  , optArtist       :: Maybe String
  , optComment      :: Maybe String
  , optGenre        :: Maybe String
  , optTitle        :: Maybe String
  , optTrack        :: Maybe Integer
  , optYear         :: Maybe Integer
  , optOutputFormat :: OutputFormat
  , optVerbose      :: Bool
  , optPaths        :: [FilePath]
  }

data OutputFormat = FmtRec | FmtTsv deriving Eq

data Info = Info {
    album   :: String
  , artist  :: String
  , comment :: String
  , genre   :: String
  , title   :: String
  , track   :: Integer
  , year    :: Integer
  }

data TsvRecord = TsvRecord {
    tpath    :: !FilePath
  , ttrack   :: !Integer
  , ttitle   :: !String
  , talbum   :: !String
  , tartist  :: !String
  , tgenre   :: !String
  , tyear    :: !Integer
  --  Don't include comment as it is not unlikely to contain tabs
  } deriving Show


main :: IO ()
main = do
  opt <- O.execParser $ O.info (O.helper <*> parseArgs) $ O.fullDesc
           <> O.header "tag v0.2 - Command line editing of audio file tags"
  run opt

-- There appears to be no builtin way to parse an option which defaults to
-- Nothing, so we need some custom code to deal with this.
optionMaybe :: O.ReadM a -> O.Mod O.OptionFields (Maybe a) -> O.Parser (Maybe a)
optionMaybe r x = O.option (fmap Just r) (O.value Nothing <> x)

parseArgs :: O.Parser Args
parseArgs = Args
  <$> parseStr 'a' "album"
  <*> parseStr 'b' "artist"
  <*> parseStr 'c' "comment"
  <*> parseStr 'g' "genre"
  <*> parseStr 't' "title"
  <*> parseNum 'n' "track"
  <*> parseNum 'y' "year"
  <*> O.flag FmtRec FmtTsv (O.long "tsv"
      <> O.help "Output tab-separated tags")
  <*> O.switch (O.long "verbose" <> O.help "Verbose output")
  <*> O.many (O.argument O.str (O.metavar "FILES"))
  where
    parseStr = parse O.str "STR"
    parseNum = parse O.auto "NUM"
    parse r meta short name = optionMaybe r (
           O.long name
        <> O.short short
        <> O.metavar meta
        <> O.help ("Set " ++ name) )

run :: Args -> IO ()
run args | null (optPaths args) = runTsv args
         | otherwise = runTag args

runTsv :: Args -> IO ()
runTsv args = do
  csvData <- BL.getContents
  case decodeByNameWith tsvopt csvData of
    Left err -> putStrLn err
    Right (_, v) -> V.forM_ v $ handleTsvRecord args
  where
    tsvopt = defaultDecodeOptions { decDelimiter = fromIntegral (ord '\t') }

handleTsvRecord :: Args -> TsvRecord -> IO ()
handleTsvRecord args tsv = do
  mfile <- T.open (tpath tsv)
  mtag  <- maybe (return Nothing) T.tag mfile
  case mtag of
    Nothing  -> if optVerbose args
                   then putStrLn ("missing tag: " ++ tpath tsv)
                   else return ()
    Just tag -> do
      modifyTag tag ( args {
            optTrack  = Just $ ttrack tsv
          , optTitle  = Just $ ttitle tsv
          , optAlbum  = Just $ talbum tsv
          , optArtist = Just $ tartist tsv
          , optGenre  = Just $ tgenre tsv
          , optYear   = Just $ tyear tsv
          } )
      T.save $ fromJust mfile
      return ()

runTag :: Args -> IO ()
runTag args = do
  when (optOutputFormat args == FmtTsv
      && (optVerbose args || not (anySetters args))) $
    putStrLn tsvHeader
  forM_ (optPaths args) $ \fname -> do
    mfile <- T.open fname
    mtag  <- maybe (return Nothing) T.tag mfile
    case mtag of
      Nothing  -> if optVerbose args then putStrLn ("missing tag: " ++ fname)
                                     else return ()
      Just tag -> handleTagFile fname (fromJust mfile) tag args

handleTagFile :: FilePath -> T.TagFile -> T.Tag -> Args -> IO ()
handleTagFile path file tag args = do
  when (optVerbose args || not (anySetters args)) $
    record (optOutputFormat args) path <$> parseInfo tag >>= putStrLn
  when (anySetters args) $ do
    modifyTag tag args
    T.save file
    return ()
  where
    record FmtRec = prettyInfo
    record FmtTsv = tsvFromInfo

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

prettyInfo :: FilePath -> Info -> String
prettyInfo path (Info alb art cmt gen tle trk yr) = concat [
      line "path:    " path
    , line "artist:  " art
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

tsvFromInfo :: FilePath -> Info -> String
tsvFromInfo path (Info alb art _ gen tle trk yr) =
    path ++ "\t" ++ show trk ++ "\t" ++ tle ++ "\t" ++ alb ++ "\t" ++ art
    ++ "\t" ++ gen ++ "\t" ++ show yr

tsvHeader = "path\ttrack\ttitle\talbum\tartist\tgenre\tyear"

instance FromNamedRecord TsvRecord where
  parseNamedRecord r = TsvRecord
      <$> r .: "path"
      <*> r .: "track"
      <*> r .: "title"
      <*> r .: "album"
      <*> r .: "artist"
      <*> r .: "genre"
      <*> r .: "year"
