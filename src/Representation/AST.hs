module Representation.AST where

import "this" Prelude

import Representation.Location


type Program = [WithLocation Statement]

data Statement
  = Assign Text (WithLocation Expression)
  | Print (WithLocation Expression)
  deriving Show

data Expression
  = VariableName     Text
  | IntLiteral       Int
  | StringExpression [StringElement]
  | Addition         (WithLocation Expression) (WithLocation Expression)
  | Subtraction      (WithLocation Expression) (WithLocation Expression)
  | Multiplication   (WithLocation Expression) (WithLocation Expression)
  | Division         (WithLocation Expression) (WithLocation Expression)
  | Modulo           (WithLocation Expression) (WithLocation Expression)
  deriving Show

data StringElement
  = StringLiteral Text
  | Interpolation (WithLocation Expression)
  deriving Show
