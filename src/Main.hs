module Main where

import "this" Prelude

import Control.Monad.Loops     (unfoldM)
import Data.HashMap.Strict     qualified as M
import Data.Text               qualified as T
import Data.Text.IO            qualified as T
import Parsing.Lexer
import Parsing.Monad
import Parsing.Parser
import Representation.AST
import Representation.Location
import Representation.Token
import System.Environment


-- parse

lex :: FilePath -> Text -> Either ParseError [(Location, Token)]
lex = runParser $ unfoldM $ fmap filterOutEOF alexGetNextToken
  where
    filterOutEOF = \case
      (_, TEOF) -> Nothing
      tokeninfo -> Just tokeninfo

parse :: FilePath -> Text -> Either ParseError Program
parse = runParser programParser


-- execute

data Value
  = StringValue Text
  | IntValue    Int

type Variables = HashMap Text Value

errorWith :: Location -> String -> a
errorWith Location {..} msg = error $ concat
  [ _locFilename
  , ":"
  , show _locLine
  , ":"
  , show _locColumn
  , ": "
  , msg
  ]

execute :: Program -> IO ()
execute = flip evalStateT M.empty . traverse_ step
  where
    step (WithLocation _ stmt) = case stmt of
      Assign var expr -> do
        value <- evaluate expr
        modify $ M.insert var value
      Print expr -> do
        value <- evaluate expr
        liftIO $ case value of
          IntValue    i -> print i
          StringValue s -> T.putStrLn s

evaluate
  :: MonadState Variables m
  => WithLocation Expression
  -> m Value
evaluate (WithLocation loc expr) = case expr of
  Addition         lhs rhs -> binary (+) evalInt     lhs rhs
  Subtraction      lhs rhs -> binary (-) evalInt     lhs rhs
  Multiplication   lhs rhs -> binary (*) evalInt     lhs rhs
  Division         lhs rhs -> binary div evalNonZero lhs rhs
  Modulo           lhs rhs -> binary mod evalNonZero lhs rhs
  VariableName     var     -> lookupVar var
  IntLiteral       val     -> pure $ IntValue val
  StringExpression elts    -> evalString elts
  where
    binary op f lhs rhs = do
      x <- evalInt lhs
      y <- f rhs
      pure $ IntValue $ op x y
    lookupVar var = do
      gets (M.lookup var) `onNothingM`
        errorWith loc ("variable not found: " ++ T.unpack var)
    evalString elts = do
      StringValue . T.concat <$> for elts \case
        StringLiteral s ->
          pure s
        Interpolation e -> evaluate e >>= \case
            IntValue    i -> pure $ T.pack $ show i
            StringValue s -> pure s
    evalInt e =
      evaluate e >>= \case
        IntValue i ->
          pure i
        StringValue s ->
          errorWith (_location e) $ "expected int got string \"" ++ T.unpack s ++ "\""
    evalNonZero e =
      evalInt e >>= \case
        0 -> errorWith (_location e) "division by zero"
        n -> pure n


-- main

main :: IO ()
main = do
  [filename] <- getArgs
  source <- T.readFile filename
  program <- parse filename source `onLeft` error
  execute program
