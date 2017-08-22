{-# LANGUAGE CPP, OverloadedStrings, TemplateHaskell #-}
module Obelisk.Asset.Serve where

import Obelisk.Asset.Accept

import Snap
import Snap.Util.FileServe

import Control.Applicative
import Control.Exception (try, throwIO, catchJust)
import Control.Monad
import Control.Monad.IO.Class
import Data.Attoparsec.ByteString (parseOnly, endOfInput)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List
import Data.Monoid
import qualified Data.Text as T
import Data.Text.Encoding
import System.Directory
import System.FilePath
import System.IO.Error
import System.Posix (getFileStatus, fileSize)

cachePermanently :: MonadSnap m => m ()
cachePermanently = do
  modifyResponse $ setHeader "Cache-Control" "public, max-age=315360000"
  modifyResponse $ setHeader "Expires" "Tue, 01 Feb 2050 00:00:00 GMT" --TODO: This should be set to "approximately one year from the time the response is sent"

doNotCache :: MonadSnap m => m ()
doNotCache = do
  modifyResponse $ setHeader "Cache-Control" "no-cache, no-store, must-revalidate" 
  modifyResponse $ setHeader "Expires" "0"

serveAssets :: MonadSnap m => FilePath -> FilePath -> m ()
serveAssets = serveAssets' True

serveAssetsInPlace :: MonadSnap m => FilePath -> FilePath -> m ()
serveAssetsInPlace = serveAssets' False

serveAssets' :: MonadSnap m => Bool -> FilePath -> FilePath -> m ()
serveAssets' doRedirect base fallback = do
  pRaw <- getSafePath
  let go p = do
        assetType <- liftIO $ try $ BS.readFile $ base </> p </> "type"
        case assetType of
          Right "immutable" -> do
            conditionalOnModification <- getsRequest $ getHeader "If-Modified-Since"
            case conditionalOnModification of
              Nothing -> do
                encodedFiles <- liftM (filter (`notElem` [".", ".."])) $ liftIO $ getDirectoryContents $ base </> p </> "encodings"
                availableEncodings <- liftM (map snd . sort) $ forM encodedFiles $ \f -> do
                  stat <- liftIO $ getFileStatus $ base </> p </> "encodings" </> f
                  return (fileSize stat, Encoding $ encodeUtf8 $ T.pack f)
                acceptEncodingRaw <- getsRequest $ getHeader "Accept-Encoding"
                ae <- case acceptEncodingRaw of
                  Nothing -> return missingAcceptableEncodings
                  Just aer -> case parseOnly (acceptEncodingBody <* endOfInput) aer of
                    Right ae -> return ae
                    Left err -> error err
                Just (Encoding e) <- return $ chooseEncoding availableEncodings ae
                modifyResponse $ setHeader "Content-Encoding" e . setHeader "Vary" "Accept-Encoding"
                if doRedirect then cachePermanently else doNotCache --TODO: Use Etags when not redirecting
                let finalFilename = base </> p </> "encodings" </> T.unpack (decodeUtf8 e)
                stat <- liftIO $ getFileStatus finalFilename
                modifyResponse $ setHeader "Last-Modified" "Thu, 1 Jan 1970 00:00:00 GMT"
                modifyResponse $ setResponseCode 200 . setContentLength (fromIntegral $ fileSize stat) . setContentType (fileType defaultMimeTypes p)
                sendFile finalFilename
              Just _ -> cachePermanently >> modifyResponse (setResponseCode 304)
          Right "redirect" -> do
            mtarget <- liftIO $ getAssetTarget $ base </> p
            case mtarget of
              Just target -> if doRedirect
                             then do
                               doNotCache
                               redirect target
                             else go $ takeDirectory p </> T.unpack (decodeUtf8 target)
              Nothing -> serveFile $ fallback </> p
          Right unknown -> error $ T.unpack ("serveAssets': Unknown asset " <> decodeUtf8 unknown)
          Left err | isDoesNotExistError err -> (doNotCache >> serveFileIfExists (fallback </> p)) <|> do
                       let (dirname, filename) = splitFileName p
                           unhashedFilename = drop 1 $ dropWhile (/= '-') filename
                       if null unhashedFilename then pass else do
                         doNotCache
                         serveFileIfExists $ fallback </> dirname </> unhashedFilename
                   | otherwise -> liftIO $ throwIO err
  go $ if "/" `isSuffixOf` pRaw || pRaw == "" then pRaw <> "index.html" else pRaw

serveFileIfExists :: MonadSnap m => FilePath -> m ()
serveFileIfExists f = do
  exists <- liftIO $ doesFileExist f
  if exists then serveFile f else pass

serveFileIfExistsAs :: MonadSnap m => ByteString -> FilePath -> m ()
serveFileIfExistsAs mimeType f = do
  exists <- liftIO $ doesFileExist f
  if exists then serveFileAs mimeType f else pass

getAssetTarget :: FilePath -> IO (Maybe ByteString)
getAssetTarget p = catchJust (\e -> if isDoesNotExistErrorType (ioeGetErrorType e) then Just () else Nothing) (liftM Just $ BS.readFile $ p </> "target") (\_ -> return Nothing)

getAssetPath :: FilePath -> FilePath -> IO (Maybe FilePath)
getAssetPath base p = do
  target <- getAssetTarget $ base </> p
  return $ fmap ((takeDirectory p </>) . T.unpack . decodeUtf8) target
