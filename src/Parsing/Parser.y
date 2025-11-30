{
module Parsing.Parser where

import "this" Prelude hiding (getChar)

import Control.Lens (over)
import Data.List.NonEmpty ((<|), singleton)
import Data.List.NonEmpty qualified as NE
import Data.Text qualified as T
import Parsing.Lexer
import Parsing.Monad
import Representation.AST
import Representation.Location
import Representation.Token

}


%name programParser program
%name expressionParser expression
%tokentype { (Location, Token) }

%error { happyError }
%errorhandlertype explist

%monad { Parser } { >>= } { return }
%lexer { lexer } { (_, TEOF) }

%left "+" "-"
%left "*" "/" "%"

%token

"print"    { ($$, TKeywordPrint)         }
";"        { ($$, TOperatorSemicolon)    }
"="        { ($$, TOperatorAssign)       }
"+"        { ($$, TOperatorAdd)          }
"-"        { ($$, TOperatorAdd)          }
"*"        { ($$, TOperatorMul)          }
"/"        { ($$, TOperatorDiv)          }
"%"        { ($$, TOperatorMod)          }
"("        { ($$, TDelimiterParensOpen)  }
")"        { ($$, TDelimiterParensClose) }
"{"        { ($$, TDelimiterBracesOpen)  }
"}"        { ($$, TDelimiterBracesClose) }

QUOTES     { ($$, TDelimiterString)      }

INT        { (_, TLiteralInt    _) }
CHAR       { (_, TLiteralChar   _) }
IDENTIFIER { (_, TIdentifier    _) }

%%

program :: { Program }
  : many(statement) { $1 }

statement :: { WithLocation Statement }
  : assign ";" { $1 }
  | print  ";" { $1 }

assign :: { WithLocation Statement }
  : IDENTIFIER "=" expression { fmap (flip Assign $3) (getIdentifier $1) }

print :: { WithLocation Statement }
  : "print" expression { WithLocation $1 (Print $2) }

expression :: { WithLocation Expression }
  : "(" expression ")"        { $2 }
  | IDENTIFIER                { fmap VariableName (getIdentifier $1) }
  | INT                       { fmap IntLiteral   (getInt        $1) }
  | expression "+" expression { binaryExpr Addition       $1 $3 }
  | expression "-" expression { binaryExpr Subtraction    $1 $3 }
  | expression "*" expression { binaryExpr Multiplication $1 $3 }
  | expression "/" expression { binaryExpr Division       $1 $3 }
  | expression "%" expression { binaryExpr Modulo         $1 $3 }
  | string_expression         { $1 }

string_expression :: { WithLocation Expression }
  : QUOTES many(string_element) QUOTES { WithLocation $1 $ StringExpression $2 }

string_element :: { StringElement }
  : "{" expression "}"  { Interpolation $2 }
  | many1(char_literal) { StringLiteral (T.pack $1) }

char_literal :: { Char }
  : CHAR { getChar $1 }

many(p)
  :           { []         }
  | many(p) p { $1 <> [$2] }

many1(p)
  : p many(p) { $1 : $2 }

{

lexer :: ((Location, Token) -> Parser a) -> Parser a
lexer = (>>=) alexGetNextToken

getIdentifier :: (Location, Token) -> WithLocation Text
getIdentifier (l, tok) = case tok of
  TIdentifier i -> WithLocation l i
  _             -> error "identifier literal is not an identifier"

getInt :: (Location, Token) -> WithLocation Int
getInt (l, tok) = case tok of
  TLiteralInt i -> WithLocation l i
  _             -> error "int literal is not an int"

getChar :: (Location, Token) -> Char
getChar (_, tok) = case tok of
  TLiteralChar c -> c
  _              -> error "char literal is not a char"

binaryExpr
  :: (WithLocation Expression -> WithLocation Expression -> Expression)
  -> WithLocation Expression
  -> WithLocation Expression
  -> WithLocation Expression
binaryExpr cons exp1 exp2 = WithLocation (_location exp1) (cons exp1 exp2)

}
