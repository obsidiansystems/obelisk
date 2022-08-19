{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}

-- | Module containing parsers and utilities for managing @Accept-Encoding@ headers and the overall process of encoding selection.
--
-- See https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.3
module Obelisk.Asset.Accept
  ( AcceptableEncodings(..)
  , Encoding(..)
  , QValue(..)
  , QValueResolution
  , acceptEncodingBody
  , chooseEncoding
  , missingAcceptableEncodings
  ) where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 710
import Prelude hiding (takeWhile, sequence)
import Data.Traversable (sequence)
#elif __GLASGOW_HASKELL__ < 810
import Data.Monoid ((<>))
#else
import Prelude hiding (takeWhile)
#endif
#else
import Prelude hiding (takeWhile)
#endif

import Control.Applicative ((<|>), optional)
import Control.Arrow (second)
import Control.Monad (replicateM, void)
import Data.Attoparsec.ByteString as AttoBS
import Data.Attoparsec.ByteString.Char8 as AC8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Either (partitionEithers)
import Data.Fixed (E3, Fixed(..), resolution)
import Data.List (sort)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Ord (Down(..))
import Data.Proxy (Proxy(..))
import Data.String (IsString)
import Data.Word (Word8)


-- | Type of a particular named encoding technique, such as @gzip@.
--
-- See https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.5
newtype Encoding = Encoding { unEncoding :: ByteString } deriving (Show, Read, Eq, Ord, IsString)

-- | Maximum precision of a Q value, in particular 3 decimal places of precision as given by the standard.
type QValueResolution = E3

-- | Type of an HTTP @qvalue@ or quality value indicating how preferred some encoding is relative to some other one.
--
-- See https://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.9
newtype QValue = QValue { unQValue :: Fixed QValueResolution } deriving (Show, Read, Eq, Ord)

-- | Structure used for picking a mutually acceptable encoding, holding a default 'QValue' along with 'QValue's for a number of 'Encoding's and typically
-- represented in HTTP as a string like gzip; q=1.0, identity; q=0.5, *; q=0.0@.
--
-- See https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.3
data AcceptableEncodings = AcceptableEncodings
  { _acceptableEncodings_defaultQValue :: QValue
  -- ^The 'QValue' associated with the default encoding value @*@, indicating any unmentioned encoding.
  , _acceptableEncodings_byEncoding :: Map Encoding QValue
  -- ^'QValue's for each 'Encoding'.
  } deriving (Show, Read, Eq, Ord)

-- | When no Accept-Encoding header is present, prefer identity, then gzip or compress, then anything else available.
missingAcceptableEncodings :: AcceptableEncodings
missingAcceptableEncodings = AcceptableEncodings
  { _acceptableEncodings_defaultQValue = QValue 0.001
  , _acceptableEncodings_byEncoding = Map.fromList
      [ (Encoding "identity", QValue 1)
      , (Encoding "gzip", QValue 0.5)
      , (Encoding "compress", QValue 0.5)
      ]
  }

-- | Takes a list of 'Encoding's and an 'AcceptableEncodings' representing preferences and returns @Just 'Encoding'@ to use of the given list based on those
-- preferences. An 'Encoding' is preferred if it has a higher 'QValue' or in the case of ties if it comes first in the given list of encodings. An encoding
-- with a @QValue@ of 0 will never be chosen. If no encoding could be chosen, either because no encodings were given or because 0 @QValue@s suppressed them,
-- then @Nothing@ is returned.
--
-- See https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.3
chooseEncoding :: [Encoding] -> AcceptableEncodings -> Maybe Encoding
chooseEncoding es ae = fmap snd $ listToMaybe $ sort $ catMaybes $ zipWith f es [(1::Int)..]
  where f e n = case encodingQValue e ae of
          QValue 0 -> Nothing
          q -> Just ((Down q, n), e) -- Choose by quality first (in descending order), then by the server's preference order

-- | Helper function for 'chooseEncoding' that returns the default AcceptableEncoding if requested encoding is unavailable
encodingQValue :: Encoding -> AcceptableEncodings -> QValue
encodingQValue e ae = Map.findWithDefault (_acceptableEncodings_defaultQValue ae) e $ _acceptableEncodings_byEncoding ae

-- | Attoparsec 'Parser' for parsing an 'AcceptableEncodings' from the value of an @Accept-Encoding@ header, as specified by HTTP/1.1 / RFC2616.
--
-- See https://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html#sec14.3
acceptEncodingBody :: Parser AcceptableEncodings
acceptEncodingBody = do
  l <- hashRule (Just 1) Nothing $ do -- partially applied hashRule awaiting to be passed Parser a
    c <- (Nothing <$ literal "*") <|> (Just <$> contentCoding) -- Parser Nothing <|> Parser Encoding
    mq <- optional $ do
      literal ";"
      literal "q"
      literal "="
      qvalue
    let q = fromMaybe (QValue 1) mq
    return (c, q)
  let (stars, specificEncodings) = partitionEithers $ flip map l $ \(c, q) -> case c of
        Nothing -> Left q
        Just n -> Right (n, q)
  starQValue <- case stars of
    [] -> return Nothing
    [q] -> return $ Just q
    _ -> fail "acceptEncodingBody: multiple * values provided"
  byEncodingProvided <- sequence $ Map.fromListWithKey (\k _ _ -> fail $ "acceptEncodingBody: encoding " <> show k <> " repeated multiple times") $ map (second return) specificEncodings
  let defaultIdentityQValue = fromMaybe (QValue 1) starQValue -- identity has a default qvalue of 1 unless * is given a different qvalue explicitly
      defaultQValue = fromMaybe (QValue 0) starQValue
      byEncoding = Map.filter (/= defaultQValue) -- Canonicalize: qvalues equal to the default are redundant
                 . Map.alter (Just . fromMaybe defaultIdentityQValue) "identity" -- Add implicit "identity" encoding, unless it has been explicitly added
                 $ byEncodingProvided
  return $ AcceptableEncodings defaultQValue byEncoding

-- | Parser for a 'QValue'.
--
-- See http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.9
qvalue :: Parser QValue
qvalue = do
  skipMany lws
  q0 <|> q1
  where q0 = do
          _ <- char '0'
          decimals <- option [] $ do
            _ <- char '.'
            starRule (Just 0) (Just numAllowedDigits) digit
          return $ QValue $ MkFixed $ fromIntegral $ (read ('0' : decimals) :: Int) * 10 ^ (numAllowedDigits - length decimals)
        q1 = do
          _ <- char '1'
          option () $ do
            _ <- char '.'
            _ <- starRule (Just 0) (Just numAllowedDigits) $ char '0'
            return ()
          return $ QValue 1
        numAllowedDigits :: Int
        numAllowedDigits = fromIntegral $ resolution (Proxy :: Proxy QValueResolution)

-- | Helper function used in 'acceptEncodingBody' to evaluate Encoding as ByteString
contentCoding :: Parser Encoding
contentCoding = Encoding <$> token

-- | Helper function used in 'contentCoding' that skips lightweight spaces and consumes input while 'isTokenChar' returns True
token :: Parser ByteString
token = skipMany lws >> AttoBS.takeWhile1 isTokenChar

-- | Helper function used in 'acceptEncodingBody', discards evaluation and returns  a 'Parser ()' to continue parsing
literal :: ByteString -> Parser ()
literal s = void $ skipMany lws >> string s

-- | Helper function used in 'token' to validate if ByteStringChar is printable but not a control or seperator char
isTokenChar :: Word8 -> Bool
isTokenChar c = isChar c && not (isCtl c || isSeparator c)

-- | Helper function used in 'isTokenChar' to validate if ByteStringChar is a control or printable char
isChar :: Word8 -> Bool
isChar c = c <= 127

-- | Helper function used in 'isTokenChar' to validate if ByteStringChar is a control char
isCtl :: Word8 -> Bool
isCtl c = c <= 31 || c == 127

-- | Helper function used in 'isTokenChar' to validate if ByteStringChar is a seperator char
isSeparator :: Word8 -> Bool
isSeparator c = BS.elem c $ spVal `BS.cons` htVal `BS.cons` "()<>@,;:\\\"/[]?={}"

-- | A version of 'liftM2' that is strict in the result of its first
-- action.
liftM2' :: (Monad m) => (a -> b -> c) -> m a -> m b -> m c
liftM2' f a b = do
  !x <- a
  y <- b
  return (f x y)
{-# INLINE liftM2' #-}

-- | See http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.1
starRule :: Maybe Int -> Maybe Int -> Parser a -> Parser [a]
starRule minNum maxNum element = do
  let numMandatory = fromMaybe 0 minNum
  mandatoryVals <- replicateM numMandatory element
  optionalVals <- case maxNum of
    Nothing -> many' element
    Just n -> do
      let countUpTo 0 _ = return []
          countUpTo m a = liftM2' (:) a (countUpTo (pred m) a) <|> return []
      countUpTo (n - numMandatory) element
  return $ mandatoryVals ++ optionalVals

-- | See http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.1
-- The spec is a bit ambiguous on whether extra commas and whitespace are permitted before and after the elements; this implementation permits them
hashRule :: forall a. Maybe Int -> Maybe Int -> Parser a -> Parser [a]
hashRule minNum maxNum element = do
  let numMandatory = fromMaybe 0 minNum
      sep :: Parser ()
      sep = do
        skipMany1 $ do
          skipMany lws
          char ','
        return ()
      processMandatory :: Bool -> Int -> Parser [a]
      processMandatory isInitial 0 = processOptional isInitial $ fmap (subtract numMandatory) maxNum
      processMandatory isInitial n = do
        if isInitial then void $ optional sep else sep
        liftM2' (:) (skipMany lws >> element) $ processMandatory False $ pred n
      processOptional :: Bool -> Maybe Int -> Parser [a]
      processOptional _ (Just 0) = return []
      processOptional isInitial n = (<|> return []) $ do
        if isInitial then void $ optional sep else sep
        liftM2' (:) (skipMany lws >> element) $ processOptional False $ fmap pred n
  result <- processMandatory True numMandatory
  _ <- optional sep
  skipMany lws
  return result

-- | Linear whitespace
-- See http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.2
lws :: Parser ()
lws = do
  option () $ cr >> lf
  _ <- starRule (Just 1) Nothing $ sp <|> ht
  return ()

-- | See http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.2
cr :: Parser ()
cr = void $ word8 13

-- | See http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.2
lf :: Parser ()
lf = void $ word8 10

-- | See http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.2
sp :: Parser ()
sp = void $ word8 spVal

spVal :: Word8
spVal = 32

-- | See http://www.w3.org/Protocols/rfc2616/rfc2616-sec2.html#sec2.2
ht :: Parser ()
ht = void $ word8 htVal

htVal :: Word8
htVal = 9
