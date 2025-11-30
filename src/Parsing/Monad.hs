{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-redundant-constraints #-}

module Parsing.Monad where

import "this" Prelude

import Control.Lens            hiding (Context, below, contexts)
import Data.List.NonEmpty      qualified as NE
import Data.Text               qualified as T
import Data.Word               (Word8)

import Representation.Location
import Representation.Token


-- Parser monad

newtype Parser a = Parser (ParserState -> Either ParseError (a, ParserState))
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadState ParserState
    , MonadError ParseError
    ) via (StateT ParserState (Except ParseError))

runParser :: Parser a -> FilePath -> Text -> Either ParseError a
runParser (Parser f) filename source = fmap fst $ f $ initialState filename source


-- internal state

type Context = Int

data ParserState = ParserState
  { _psInput    :: Text
  , _psLocation :: Location
  , _psPrevChar :: Char
  , _psContexts :: NonEmpty Context
  } deriving Show

initialState :: FilePath -> Text -> ParserState
initialState filename source = ParserState
  { _psInput    = source
  , _psLocation = initialLocation filename
  , _psPrevChar = '\n'
  , _psContexts = pure 0
  }


-- error

type ParseError = String


-- lens generation

makeLenses ''ParserState


-- common functions

parseError :: String -> Parser a
parseError message = do
  Location filename _ line column <- use psLocation
  throwError $ concat
    [ filename
    , ":"
    , show line
    , ":"
    , show column
    , ": parse error: "
    , message
    ]


-- alex functions

type AlexInput = ParserState

alexGetChar :: ParserState -> Maybe (Char, ParserState)
alexGetChar prev@ParserState {..} = do
  (c, remaining) <- T.uncons _psInput
  let newPos   = updateLocation _psLocation c
      newState = prev
        { _psLocation = newPos
        , _psInput    = remaining
        , _psPrevChar = c
        }
  pure (c, newState)

alexGetByte :: ParserState -> Maybe (Word8, ParserState)
alexGetByte prevState = do
  (c, newState) <- alexGetChar prevState
  pure (fromIntegral (ord c), newState)

alexNextChar :: Parser Char
alexNextChar = do
  prevState <- get
  case alexGetChar prevState of
    Nothing -> parseError "unexpected end of input"
    Just (c, newState) -> do
      put newState
      pure c

alexInputPrevChar :: ParserState -> Char
alexInputPrevChar = view psPrevChar

alexError :: Parser a
alexError = parseError "lexical error"

alexPush :: Context -> Parser ()
alexPush ctx = do
  psContexts %= (NE.<|) ctx

alexPop :: Parser ()
alexPop = do
  (_, below) <- uses psContexts NE.uncons
  case below of
    Just contexts ->
      psContexts .= contexts
    Nothing ->
      parseError "internal error: tried to terminate root expression"

alexEscapedChar :: Parser Char
alexEscapedChar = do
  alexNextChar >>= \case
    'n'  -> pure '\n'
    'r'  -> pure '\r'
    't'  -> pure '\t'
    '0'  -> pure '\0'
    '"'  -> pure '"'
    c    -> parseError $ "unrecognized escaped character \'" ++ [c] ++ "'"


-- happy functions

happyError :: ((Location, Token), [String]) -> Parser a
happyError ((_, token), expected) =
  parseError $ concat
    [ "expecting one of: "
    , unwords expected
    , ", but got: "
    , show token
    ]
