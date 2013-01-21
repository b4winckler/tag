module Main where

import Control.Applicative ((<$>), (<*>))
import Control.Monad (forM_, when)
import Data.Monoid
import Data.Maybe (catMaybes, maybe, fromJust)
import Options.Applicative

import qualified Sound.TagLib as TagLib

data Args = Args {
    optAlbum   :: Maybe String
  , optArtist  :: Maybe String
  , optComment :: Maybe String
  , optGenre   :: Maybe String
  , optTitle   :: Maybe String
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
  <*> switch (long "verbose" <> help "Verbose output")
  <*> arguments1 str (metavar "FILES")

maybeStrOption m = option (reader (return . Just) <> value Nothing <> m)

run :: Args -> IO ()
run args = forM_ (optPaths args) $ \fname -> do
  mfile <- TagLib.open fname
  mtag  <- maybe (return Nothing) TagLib.tag mfile
  case mtag of
    Nothing  -> putStrLn ("missing tag: " ++ fname) >> return ()
    Just tag -> handle mfile tag args

handle file tag args = do
  when (optVerbose args || not (anySetters args)) $ parseInfo tag >>= print
  when (anySetters args) $ do
    modifyTag tag args
    TagLib.save (fromJust file)
    return ()

modifyTag tag args = do
  withJust (optAlbum   args) (TagLib.setAlbum   tag)
  withJust (optArtist  args) (TagLib.setArtist  tag)
  withJust (optComment args) (TagLib.setComment tag)
  withJust (optGenre   args) (TagLib.setGenre   tag)
  withJust (optTitle   args) (TagLib.setTitle   tag)
  where
    withJust m a = maybe (return ()) a m

anySetters :: Args -> Bool
anySetters args = not $ null $ catMaybes $
  [optAlbum, optArtist, optComment, optGenre, optTitle] <*> [args]

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

{-
run2 :: Args -> IO ()
run2 args = do
  let allPaths = optPaths args
  (good, badPaths) <- partitionFiles allPaths <$> mapM readInfo allPaths

  let nbad = length badPaths
  when (optVerbose args && nbad > 0) $ do
    putStrLn $ "Ignoring " ++ show nbad ++ " files:\n"
    forM_ badPaths $ putStrLn
    putStrLn ""

  let ngood = length good
  when (optVerbose args && ngood > 0) $ do
    putStrLn "Original tags:\n"
    forM_ (snd $ unzip good) $ putStrLn . show
    putStrLn ""

  when (anySetters args) $ do
    let modified = tag args good
    putStrLn $ "Modified tags:\n"
    forM_ (snd $ unzip modified) $ putStrLn . show
    putStrLn ""


readInfo :: FilePath -> IO (Maybe Info)
readInfo path = do
  mfile <- TagLib.open path
  mtag  <- maybe (return Nothing) TagLib.tag mfile
  maybe (return Nothing) (parseInfo >=> return . Just) mtag

readInfo' :: FilePath -> IO (Maybe Info)
readInfo' path = do
  mf <- TagLib.open path
  case mf of
    Nothing -> return Nothing
    Just tagFile -> do
      mt <- TagLib.tag tagFile
      case mt of
        Nothing -> return Nothing
        Just tag -> do
          info <- parseInfo tag
          return $ Just info

tag :: Args -> [(FilePath, Info)] -> [(FilePath, Info)]
tag args = map modify
  where
    modify (path, info) = (path, info {
        album   = maybe (album info)   id (optAlbum   args)
      , artist  = maybe (artist info)  id (optArtist  args)
      , comment = maybe (comment info) id (optComment args)
      , genre   = maybe (genre info)   id (optGenre   args)
      , title   = maybe (title info)   id (optTitle   args)
      })


-- Collect files which have tags and separate them from the files without.
partitionFiles :: [FilePath] -> [Maybe Info] -> ([(FilePath, Info)], [FilePath])
partitionFiles paths infos = ( map (\(a,b) -> (a, fromJust b)) good
                             , map fst bad )
  where
    (good, bad) = partition (isJust . snd) (zip paths infos)

-}
