module Representation.Token where

import "this" Prelude

data Token
  = TKeywordPrint
  | TOperatorSemicolon
  | TOperatorAssign
  | TOperatorAdd
  | TOperatorSub
  | TOperatorMul
  | TOperatorDiv
  | TOperatorMod
  | TDelimiterString
  | TDelimiterParensOpen
  | TDelimiterParensClose
  | TDelimiterBracesOpen
  | TDelimiterBracesClose
  | TLiteralInt Int
  | TLiteralChar Char
  | TIdentifier Text
  | TEOF
  deriving Show
