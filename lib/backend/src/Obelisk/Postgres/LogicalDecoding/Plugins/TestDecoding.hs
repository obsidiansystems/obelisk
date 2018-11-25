{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Obelisk.Postgres.LogicalDecoding.Plugins.TestDecoding
  ( line
  , Transaction
  , Change (..)
  , LineError (..)
  , linesToTransactions
  ) where

import Prelude hiding (takeWhile)

import Control.Applicative
import Control.Monad
import Data.Attoparsec.ByteString (word8, notWord8)
import Data.Attoparsec.ByteString.Char8
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import Data.Char (ord)
import qualified Data.Char as Char
import Data.Word
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Sequence (Seq, (|>))
import Data.Text (Text)
import Data.Text.Encoding
import Database.PostgreSQL.Simple.Types
import qualified Data.Map as Map
import Data.Map (Map)
import Data.IORef
import Data.Maybe

--------------------------------------------------------------------------------
-- Potentially upstreamable
--------------------------------------------------------------------------------

stringWithSingleQuote :: Parser ByteString
stringWithSingleQuote = stringWithQuote $ fromIntegral $ ord '\''

stringWithDoubleQuote :: Parser ByteString
stringWithDoubleQuote = stringWithQuote $ fromIntegral $ ord '"'

stringWithQuote :: Word8 -> Parser ByteString
stringWithQuote q = do
  _ <- word8 q
  s <- many $ notWord8 q <|> doubledWord8 q
  _ <- word8 q
  pure $ BS.pack s

doubledWord8 :: Word8 -> Parser Word8
doubledWord8 c = do
  _ <- word8 c
  _ <- word8 c
  pure c

identifierLike :: (Char -> Bool) -> (Char -> Bool) -> Parser Text
identifierLike validFirstChar validOtherChar = fmap decodeUtf8 stringWithDoubleQuote <|> do
  -- Avoid plucking the first character off, so that we don't have to cons it back on (which would reallocate the ByteString)
  guard . validFirstChar =<< peekChar'
  decodeUtf8 <$> takeWhile1 validOtherChar

identifier :: Parser Identifier
identifier = Identifier <$> identifierLike identifierFirstChar identifierOtherChar

identifierFirstChar :: Char -> Bool
identifierFirstChar c = Char.isLetter c || c == '_'

identifierOtherChar :: Char -> Bool
identifierOtherChar c = identifierFirstChar c || isDigit c || c == '$'

qualifiedIdentifier :: Parser QualifiedIdentifier
qualifiedIdentifier = do
  namespace <- identifier
  _ <- char '.'
  name <- identifier
  pure $ QualifiedIdentifier (Just $ fromIdentifier namespace) $ fromIdentifier name

newtype TypeName = TypeName { unTypeName :: Text } deriving (Show, Read, Eq, Ord)

typeName :: Parser TypeName
typeName = do
  let otherChar c = identifierOtherChar c || c `elem` (" ()," :: String)
  TypeName <$> identifierLike identifierFirstChar otherChar

newtype Xid = Xid { unXid :: Word64 } deriving (Show, Read, Eq, Ord)

xid :: Parser Xid
xid = Xid <$> decimal

--------------------------------------------------------------------------------

rowEntry :: Parser a -> Parser (Identifier, (TypeName, a))
rowEntry item = do
  colName <- identifier
  _ <- char '['
  colType <- typeName
  _ <- string "]:"
  v <- item
  pure (colName, (colType, v))

rowLike :: Parser a -> Parser (HashMap Identifier (TypeName, a))
rowLike item = {- (mempty <$ string "(no-tuple-data)") <|> -} do
  fmap HashMap.fromList $ flip sepBy1 (char ' ') $ rowEntry item

type Row = HashMap Identifier (TypeName, Maybe Literal)

data Literal
   = Literal_UnchangedToastDatum
   | Literal_Present ByteString
   deriving (Show, Read, Eq, Ord)

nullableLiteral :: Parser (Maybe Literal)
nullableLiteral = Nothing <$ string "null" <|> Just <$> literal

literal :: Parser Literal
literal = unchangedToastDatum <|> presentDatum
  where
    unchangedToastDatum = Literal_UnchangedToastDatum <$ string "unchanged-toast-datum"
    -- | We convert all present datums to their normal Postgres ByteString
    -- encoding, so that we don't have to preserve special cases throughout our
    -- code
    presentDatum :: Parser Literal
    presentDatum = Literal_Present <$> foldr1 (<|>)
      [ stringWithSingleQuote
      , char 'B' *> stringWithSingleQuote
      , "t" <$ string "true"
      , "f" <$ string "false"
      -- We assume anything else must be numeric
      , takeWhile1 (/= ' ')
      ]

row :: Parser Row
row = rowLike nullableLiteral

type Key = HashMap Identifier (TypeName, Literal) -- Keys can't contain nulls

key :: Parser Key
key = rowLike literal

data Change
   = Change_Insert Row
   | Change_Update Key Row
   | Change_Delete Key
   deriving (Show, Read, Eq, Ord)

change :: Parser Change
change = do
  t <- takeWhile (/= ':')
  _ <- string ": "
  case t of
    "INSERT" -> Change_Insert <$> row
    "UPDATE" -> foldr1 (<|>)
      [ do _ <- string "old-key: "
           old <- key
           _ <- string " new-tuple: "
           new <- row
           pure $ Change_Update old new
      , Change_Update mempty <$> row
      ]
    "DELETE" -> Change_Delete <$> key
    _ -> fail $ "Unrecognized change type: " <> show t

data Line
   = Line_Begin Xid
   | Line_Change QualifiedIdentifier Change
   | Line_Commit Xid
   deriving (Show, Read, Eq, Ord)

line :: Parser Line
line = do
  leader <- takeWhile (/= ' ')
  _ <- char ' '
  case leader of
    "BEGIN" -> Line_Begin <$> xid
    "table" -> do
      table <- qualifiedIdentifier
      _ <- string ": "
      Line_Change table <$> change
    "COMMIT" -> Line_Commit <$> xid
    _ -> fail $ "Unrecognized line leader: " <> show leader

type Transaction = (Xid, Map QualifiedIdentifier (Seq Change))

data LineError
   = LineError_OtherTransactionAlreadyInProgress
     Xid -- Existing transaction ID
     Xid -- New transaction ID that tried to start
   | LineError_CommittingWithoutTransaction
     Xid -- Transaction ID that tried to commit
   | LineError_CommittingWrongTransaction
     Xid -- Existing transaction ID
     Xid -- New transaction ID that tried to start
   | LineError_ChangeOutsideTransaction

-- | Create a function that will statefully accumulate lines until a Transaction
-- is produced.  If the given line is not valid in the current state, an error
-- will be produced and the line will be ignored.  This should never happen for
-- a valid LogicalDecoding session, so all errors should be treated as serious.
linesToTransactions :: IO (Line -> IO (Either LineError (Maybe Transaction)))
linesToTransactions = do
  pending <- newIORef Nothing
  return $ \l -> do
    let -- Update the pending transaction and possibly emit a message
        update :: Maybe Transaction -> Either LineError (Maybe Transaction, Maybe Transaction)
        update = case l of
          Line_Begin newXid -> \case
            Nothing -> Right (Just (newXid, mempty), Nothing)
            Just (existingXid, _) -> Left $ LineError_OtherTransactionAlreadyInProgress existingXid newXid
          Line_Commit newXid -> \case
            Just txn@(existingXid, _)
              | existingXid == newXid -> Right (Nothing, Just txn)
              | otherwise -> Left $ LineError_CommittingWrongTransaction existingXid newXid
            Nothing -> Left $ LineError_CommittingWithoutTransaction newXid
          Line_Change qid c -> \old -> case old of
            Just (existingXid, changes) -> Right (Just (existingXid, Map.alter (Just . (|> c) . fromMaybe mempty) qid changes), Nothing)
            Nothing -> Left LineError_ChangeOutsideTransaction
    atomicModifyIORef pending $ \old -> case update old of
      Left e -> (old, Left e)
      Right (new, a) -> (new, Right a)
