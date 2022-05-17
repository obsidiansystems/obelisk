{-# LANGUAGE OverloadedStrings #-}
-- | Serve preprocessed assets using Snap.
module Obelisk.Asset.Serve.Snap
  ( serveAssets
  , serveAssetsInPlace
  , serveAsset
  , serveAssetInPlace
  , getAssetPath
  ) where

import Obelisk.Asset.Accept (Encoding (..), acceptEncodingBody, chooseEncoding, missingAcceptableEncodings)
import Obelisk.Snap.Extras

import Snap
  (MonadSnap, getHeader, getRequest, getsRequest, modifyResponse, pass, redirect, sendFile, setContentLength, setContentType, setHeader, setResponseCode)
import Snap.Util.FileServe (fileType, getSafePath, serveFile)
import Snap.Internal.Util.FileServe (checkRangeReq)

import Control.Applicative ((<|>))
import Control.Exception (handleJust, try, throwIO)
import Control.Monad (forM, liftM, unless)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Fail (MonadFail)
import Data.Attoparsec.ByteString (parseOnly, endOfInput)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.List (isSuffixOf, sort)
import Data.Monoid ((<>))
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import System.Directory (getDirectoryContents)
import System.FilePath ((</>), splitFileName, takeDirectory)
import System.IO.Error (isDoesNotExistError)
import System.PosixCompat.Files (getFileStatus, fileSize)

-- | Serve static assets from an asset directory generated via @assets.nix@ or, failing that, from a regular directory.
--
-- For assets generated from @assets.nix@, redirects will be sent to the browser
-- from the logical asset name (e.g. @test.png@) to the unique filename for the
-- current version of the asset
-- (e.g. @0rx5yvkkgkig2pcqf4ngi1l7vh89qqajdzc6aiayaibyhvj0d853-test.png@). Hashed
-- asset names will be sent with permanent caching headers.
serveAssets :: (MonadSnap m, MonadFail m) => FilePath -> FilePath -> m ()
serveAssets = serveAssets' True

-- | Serve static assets from an asset directory generated via @assets.nix@ or, failing that, from a regular directory.
--
-- For assets generated from @assets.nix@, redirects will be not sent to the
-- browser for the logical asset name (e.g. @test.png@) but instead the current
-- version of the asset with to point the browser at the unique filename for the
-- current version of the asset.
serveAssetsInPlace :: (MonadSnap m, MonadFail m) => FilePath -> FilePath -> m ()
serveAssetsInPlace = serveAssets' False

-- | Like 'serveAssets', but only serves a single specified asset
serveAsset :: (MonadSnap m, MonadFail m) => FilePath -> FilePath -> FilePath -> m ()
serveAsset = serveAsset' True

-- | Like 'serveAssetsInPlace', but only serves a single specified asset
serveAssetInPlace :: (MonadSnap m, MonadFail m) => FilePath -> FilePath -> FilePath -> m ()
serveAssetInPlace = serveAsset' False

-- | Serve static assets from an asset directory generated via @assets.nix@ or, failing that, from a regular directory.
--
-- For assets generated from @assets.nix@, the @Bool@ argument @doRedirect@ controls whether redirects will be sent to the browser if a request is made for
-- an unhashed asset name, e.g. @test.png@. For @True@, a redirect will be sent, yielding more round trips to the server but better caching behavior if the
-- asset doesn't change often. Conversely for @False@, the asset will be served "in place" but made uncacheable.
serveAssets' :: (MonadSnap m, MonadFail m) => Bool -> FilePath -> FilePath -> m ()
serveAssets' doRedirect base fallback = do
  pRaw <- getSafePath
  serveAsset' doRedirect base fallback $ if "/" `isSuffixOf` pRaw || pRaw == "" then pRaw <> "index.html" else pRaw

-- | Serve a single static asset from an asset directory generated via @assets.nix@ or, failing that, from a regular directory.
serveAsset' :: (MonadFail m, MonadSnap m) => Bool -> FilePath -> FilePath -> FilePath -> m ()
serveAsset' doRedirect base fallback p = do
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
            . setHeader "Accept-Ranges" "bytes"
            . setContentType (fileType modernMimeTypes p)
          let size = fromIntegral $ fileSize stat
          req <- getRequest
          -- Despite the name, this function actually does all of the work for
          -- responding to range requests. We only need to handle *none* range
          -- requests ourselves.
          wasRange <- checkRangeReq req finalFilename size
          unless wasRange $ do
            modifyResponse $ setResponseCode 200 . setContentLength size
            sendFile finalFilename
        Just _ -> do
          cachePermanently >> modifyResponse (setResponseCode 304)
    Right "redirect" -> do
      mtarget <- liftIO $ getAssetTarget $ base </> p
      case mtarget of
        Just target -> if doRedirect
                       then do
                         doNotCache
                         redirect target
                       else do
                         serveAsset' doRedirect base fallback $ takeDirectory p </> T.unpack (decodeUtf8 target)
        Nothing -> do
          serveFile $ fallback </> p
    Right unknown -> error $ T.unpack ("serveAssets': Unknown asset " <> decodeUtf8 unknown)
    Left err | isDoesNotExistError err -> (doNotCache >> serveFileIfExists (fallback </> p)) <|> do
                 let (dirname, filename) = splitFileName p
                     unhashedFilename = drop 1 $ dropWhile (/= '-') filename
                 if null unhashedFilename then pass else do
                   doNotCache
                   serveFileIfExists $ fallback </> dirname </> unhashedFilename
             | otherwise -> liftIO $ throwIO err

-- | If the given file exists in a hashed location, return that location.  The
-- resulting FilePath will be relative to @base@, just like @assetPath@ is.
getAssetPath
  :: FilePath -- ^ @base@: Path to asset directory
  -> FilePath -- ^ @assetPath@: Path to non-hashed asset within the asset directory
  -> IO (Maybe FilePath) -- ^ Path to hashed asset within the asset directory, if it exists
getAssetPath base p = do
  target <- getAssetTarget $ base </> p
  return $ fmap ((takeDirectory p </>) . T.unpack . decodeUtf8) target

-- | Given a file path into an asset directory prepared by @assets.nix@, read
-- the target path from the metadata if it exists. Returns @Nothing@ when the
-- asset path doesn't exist.
getAssetTarget :: FilePath -> IO (Maybe ByteString)
getAssetTarget p =
  handleJust
    (\ e  -> if isDoesNotExistError e then Just () else Nothing)
    (\ () -> return Nothing)
    (liftM Just $ BS.readFile $ p </> "target")
