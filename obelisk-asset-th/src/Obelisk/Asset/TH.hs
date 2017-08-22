module Obelisk.Asset.TH where

import Language.Haskell.TH.Syntax
import System.FilePath
import qualified Data.Text as T
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.Lazy.Builder as LBS
import Data.Digest.Pure.SHA
import Data.Text.Encoding
import Data.Monoid
import Data.Bits
import qualified Data.Vector.Unboxed as UV
import Data.Char
import Data.Word

-- | Warning: does not support negative numbers (will result in infinite loop)
toNixBase32 :: LBS.ByteString -> LBS.ByteString
toNixBase32 x = LBS.toLazyByteString $ mconcat $ map (LBS.word8 . (symbols UV.!) . fromIntegral) vals
  where vals = byteStringToQuintets x
        symbols = UV.fromList $ map (fromIntegral . ord) $ filter (`notElem` "eotu") $ ['0'..'9'] <> ['a'..'z']


-- See https://github.com/NixOS/nix/blob/6f1743b1a5116ca57a60b481ee4083c891b7a334/src/libutil/hash.cc#L109
byteStringToQuintets :: LBS.ByteString -> [Word8]
byteStringToQuintets hash = map f [len-1, len-2 .. 0]
  where hashSize = fromIntegral $ LBS.length hash
        len = (hashSize * 8 - 1) `div` 5 + 1
        f n = let b = n * 5
                  (i, j) = b `divMod` 8
                  j' = fromIntegral j
                  c = ((hash `LBS.index` i) `shift` (-j')) .|. (if i >= hashSize - 1 then 0 else (hash `LBS.index` (i + 1)) `shift` (8 - j')) --TODO: This is probably pretty slow; replace with something that doesn't use LBS.index
              in c .&. 0x1f

assetPath' :: FilePath -> FilePath -> Q FilePath
assetPath' root relativePath = do
  let path = root </> relativePath
  qAddDependentFile path
  contents <- fmap LBS.fromStrict $ runIO $ BS.readFile path
  let hashPrefix = T.unpack $ decodeUtf8 $ LBS.toStrict $ toNixBase32 $ bytestringDigest $ sha256 contents
      (dir, filename) = splitFileName relativePath
  return $ dir </> (hashPrefix <> "-" <> filename)

assetPath :: FilePath -> FilePath -> Q Exp
assetPath root relativePath =
  LitE . StringL <$> assetPath' root relativePath
