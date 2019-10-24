{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE OverloadedStrings #-}

module Obelisk.Configs.Internal.Environment where

import Control.Applicative
import Control.Monad
import qualified Data.Attoparsec.ByteString as P
import qualified Data.Attoparsec.ByteString.Char8 as P (hexadecimal)
import qualified Data.Attoparsec.Combinator as P
import Data.Bifunctor (second)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as Base64
import qualified Data.ByteString.Builder as Builder
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Char as Char
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import Data.Tuple (swap)
import Data.Witherable
import Data.Word
import System.Environment
import qualified Text.Printf as Printf


data EnvCodec
  = EnvCodec_Char8
  | EnvCodec_Base64
  deriving (Eq, Ord, Show)

envVarEscapes :: [(Char, Word8)]
envVarEscapes = fmap (second f)
  [ ('-', 'h')
  , ('_', 'u')
  , ('.', 'd')
  ]
  where
    f :: Char -> Word8
    f = fromIntegral . Char.ord

escapeVarName :: EnvCodec -> Text -> ByteString
escapeVarName codec x = LBS.toStrict . Builder.toLazyByteString $ T.foldr ((<>) . f) mempty x <> codec'
  where
    escapeCode = Map.fromList envVarEscapes

    codec' = case codec of
      EnvCodec_Base64 -> "__base64"
      EnvCodec_Char8 -> ""

    f :: Char -> Builder.Builder
    f c
      | not (Char.isAscii c) = error "bad"
      | c == '/' = "_"
      | Char.isAlphaNum c = Builder.char7 c
      | Just c' <- Map.lookup c escapeCode = "__" <> Builder.word8 c'
      | otherwise = "__x" <> (Builder.string7 $ Printf.formatInt (Char.ord c) (Printf.FieldFormat (Just 2) Nothing (Just Printf.ZeroPad) Nothing False "" 'x') "")

unescapeVarName :: ByteString -> Either String (Text, EnvCodec)
unescapeVarName x = P.parseOnly ((,) <$> fileName <*> codec <* P.endOfInput) x
  where
    escapeCode = Map.fromList $ fmap swap envVarEscapes

    codec :: P.Parser EnvCodec
    codec = P.option EnvCodec_Char8 $ (EnvCodec_Base64 <$ "__base64")

    -- | parse ps. succeed only if both ps and qs succeeds
    guardLookahead ps qs = do
      p <- ps
      _ <- P.lookAhead qs
      return p

    parseEscapeCode =
      ("__" *> (do
        c <- P.anyWord8
        case Map.lookup c escapeCode of
          Nothing -> mzero
          Just c' -> return c')
      ) <|> ( "__x" *> (Char.chr . fromIntegral <$> P.hexadecimal @Word8))

    parseEnvChar = Char.chr . fromIntegral <$> P.satisfy (P.inClass "a-zA-Z0-9")

    fileName = LT.toStrict . LT.pack <$> P.many' (P.choice
      [ parseEnvChar
      , parseEscapeCode
      , guardLookahead ('/' <$ "_") (parseEnvChar <|> parseEscapeCode)
      ])


getConfigsFromEnvironment :: ByteString -> Map Text ByteString -> IO (Map Text ByteString)
getConfigsFromEnvironment varNamePrefix dirConfigs = do
  let
    prefix needle haystack
      | needle `BS.isPrefixOf` haystack = Just (BS.drop (BS.length needle) haystack)
      | otherwise = Nothing

    makeEnv (k0, v0) = case prefix varNamePrefix (Char8.pack k0) of
      Nothing -> return Nothing
      Just k -> case unescapeVarName k of
        Left bad -> error bad
        Right (k', codec) ->
          let
            v = case codec of
              EnvCodec_Char8 -> Char8.pack v0
              EnvCodec_Base64 -> Base64.decodeLenient (Char8.pack v0)
          in case Map.lookup k' dirConfigs of
            Just v' | v /= v' -> error $ "conflicting obelisk config: env " <> k0 <> " /= ./config/" <> (T.unpack k')
            _ -> return (Just (k', v))

  env <- wither makeEnv =<< getEnvironment
  return $ (Map.fromList env) <> dirConfigs
