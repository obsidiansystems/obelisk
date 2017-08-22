{-# LANGUAGE CPP, OverloadedStrings, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving #-}
module Focus.HTTP.Accept where

#ifdef __GLASGOW_HASKELL__
#if __GLASGOW_HASKELL__ < 710
import Prelude hiding (takeWhile, sequence)
import Control.Monad hiding (sequence)
import Data.Traversable (sequence)
#else
import Prelude hiding (takeWhile)
import Control.Monad
#endif
#else
import Prelude hiding (takeWhile)
import Control.Monad
#endif

import Control.Applicative
import qualified Data.ByteString as BS
import Data.Attoparsec.ByteString as A
import Data.Attoparsec.ByteString.Char8 as AC8
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Word (Word8)
import Data.Monoid
import Data.Fixed
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Either
import Control.Arrow
import Data.List
import Data.String
import Data.Proxy
import Data.Ord

newtype Encoding = Encoding { unEncoding :: ByteString } deriving (Show, Read, Eq, Ord, IsString)
type QValueResolution = E3 -- This resolution is given by the standard
newtype QValue = QValue { unQValue :: Fixed QValueResolution } deriving (Show, Read, Eq, Ord)

-- | When no Accept-encoding header is present, prefer identity, then gzip or compress, then anything else available
--
-- See http://www.w3.org/Protocols/rfc2616/rfc2616-sec14.html
missingAcceptableEncodings :: AcceptableEncodings
missingAcceptableEncodings = AcceptableEncodings
  { _acceptableEncodings_defaultQValue = QValue 0.001
  , _acceptableEncodings_byEncoding = Map.fromList
      [ (Encoding "identity", QValue 1)
      , (Encoding "gzip", QValue 0.5)
      , (Encoding "compress", QValue 0.5)
      ]
  }

data AcceptableEncodings
   = AcceptableEncodings { _acceptableEncodings_defaultQValue :: QValue
                         , _acceptableEncodings_byEncoding :: Map Encoding QValue
                         }
   deriving (Show, Read, Eq, Ord)

chooseEncoding :: [Encoding] -> AcceptableEncodings -> Maybe Encoding
chooseEncoding es ae = fmap snd $ listToMaybe $ sort $ catMaybes $ zipWith f es [(1::Int)..]
  where f e n = case encodingQValue e ae of
          QValue 0 -> Nothing
          q -> Just ((Down q, n), e) -- Choose by quality first (in descending order), then by the server's preference order

encodingQValue :: Encoding -> AcceptableEncodings -> QValue
encodingQValue e ae = Map.findWithDefault (_acceptableEncodings_defaultQValue ae) e $ _acceptableEncodings_byEncoding ae

acceptEncodingBody :: Parser AcceptableEncodings
acceptEncodingBody = do
  l <- hashRule (Just 1) Nothing $ do
    c <- (Nothing <$ literal "*") <|> (Just <$> contentCoding)
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

-- | See http://www.w3.org/Protocols/rfc2616/rfc2616-sec3.html#sec3.9
qvalue :: Parser QValue
qvalue = do
  skipMany lws
  q0 <|> q1
  where q0 = do
          char '0'
          decimals <- option [] $ do
            char '.'
            starRule (Just 0) (Just numAllowedDigits) digit
          return $ QValue $ MkFixed $ fromIntegral $ (read ('0' : decimals) :: Int) * 10 ^ (numAllowedDigits - length decimals)
        q1 = do
          char '1'
          option () $ do
            char '.'
            starRule (Just 0) (Just numAllowedDigits) $ char '0'
            return ()
          return $ QValue 1
        numAllowedDigits :: Int
        numAllowedDigits = fromIntegral $ resolution (Proxy :: Proxy QValueResolution)

contentCoding :: Parser Encoding
contentCoding = Encoding <$> token

token :: Parser ByteString
token = skipMany lws >> A.takeWhile1 isTokenChar

literal :: ByteString -> Parser ()
literal s = void $ skipMany lws >> string s

isTokenChar :: Word8 -> Bool
isTokenChar c = isChar c && not (isCtl c || isSeparator c)

isChar :: Word8 -> Bool
isChar c = c <= 127

isCtl :: Word8 -> Bool
isCtl c = c <= 31 || c == 127

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
  starRule (Just 1) Nothing $ sp <|> ht
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
