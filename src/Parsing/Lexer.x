{
module Parsing.Lexer where

import "this" Prelude

import Control.Monad.Extra (whenM)
import Control.Monad.Loops (whileM, unfoldM)
import Data.Char (digitToInt)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Data.Text.Read qualified as T
import Parsing.Monad
import Representation.Location hiding (location)
import Representation.Token

}

tokens :-
  <0, interpolation> \"  { stringBegin }
  <string>           \"  { stringEnd   }
  <string>           "{" { interpolationBegin }
  <interpolation>    "}" { interpolationEnd   }

  <string> "\\" { stringEscape  }
  <string> .    { stringLiteral }

  <0, comment, interpolation> "/*" { commentBegin }
  <0, comment, interpolation> "*/" { commentEnd   }
  <comment> .  ;
  <comment> \n ;

  <0, interpolation> $white+ ;

  <0> "print "           { mkToken TKeywordPrint         }
  <0> ";"                { mkToken TOperatorSemicolon    }
  <0> "="                { mkToken TOperatorAssign       }
  <0, interpolation> "+" { mkToken TOperatorAdd          }
  <0, interpolation> "-" { mkToken TOperatorSub          }
  <0, interpolation> "*" { mkToken TOperatorMul          }
  <0, interpolation> "/" { mkToken TOperatorDiv          }
  <0, interpolation> "%" { mkToken TOperatorMod          }
  <0, interpolation> "(" { mkToken TDelimiterParensOpen  }
  <0, interpolation> ")" { mkToken TDelimiterParensClose }

  <0, interpolation> [a-zA-Z]+ { mkIdentifier }
  <0, interpolation> [0-9]+    { mkIntLiteral }

{

type AlexAction = Location -> Text -> Parser (Location, Token)

mkToken :: Token -> AlexAction
mkToken tok location _ = do
  pure (location, tok)

mkIntLiteral :: AlexAction
mkIntLiteral location matched = case T.decimal matched of
  Right (intValue, remaining)
    | T.null remaining -> pure (location, TLiteralInt intValue)
  _ -> error $ "not a valid int: " ++ T.unpack matched

mkIdentifier :: AlexAction
mkIdentifier location matched = do
  pure (location, TIdentifier matched)

stringBegin :: AlexAction
stringBegin location _ = do
  alexPush string
  pure (location, TDelimiterString)

stringEnd :: AlexAction
stringEnd location _ = do
  alexPop
  pure (location, TDelimiterString)

interpolationBegin :: AlexAction
interpolationBegin location _ = do
  alexPush interpolation
  pure (location, TDelimiterBracesOpen)

interpolationEnd :: AlexAction
interpolationEnd location _ = do
  alexPop
  pure (location, TDelimiterBracesClose)

commentBegin :: AlexAction
commentBegin _ _ = do
  alexPush comment
  alexGetNextToken

commentEnd :: AlexAction
commentEnd _ _ = do
  alexPop
  alexGetNextToken

stringEscape :: AlexAction
stringEscape location _ = do
  c <- alexEscapedChar
  pure (location, TLiteralChar c)

stringLiteral :: AlexAction
stringLiteral location matched = do
  case T.uncons matched of
    Just (c, rest) | T.null rest -> pure (location, TLiteralChar c)
    _ -> error $ "didn't match exactly one character: " ++ T.unpack matched

alexGetNextToken :: Parser (Location, Token)
alexGetNextToken = do
  currentState@ParserState {..} <- get
  case alexScan currentState (NE.head _psContexts) of
    AlexEOF ->
      pure (_psLocation, TEOF)
    AlexError newState -> do
      put newState
      parseError $ "unspecified lexical error\n  old state: " ++ show currentState ++ "\n  new state: " ++ show newState
    AlexSkip  newState _len -> do
      put newState
      alexGetNextToken
    AlexToken newState len action -> do
      put newState
      let matchedText = T.take len _psInput
      action _psLocation matchedText

}
