module THISAPP.Program.Parser (
  module THISAPP.Program.Parser,
  module THISAPP.Program.Ast,
  module THISAPP.Program.ProgramSym
) where

import Prelude hiding (between)

import Control.Alt ((<|>))
import Control.Lazy (defer)
import Control.Monad.State (gets)
import Control.Plus (empty)

import Data.Array (concat, (:), snoc, some, many)
import Data.Either (Either(..))
import Data.Map (lookup)
import Data.Maybe (Maybe(..))
import Data.Maybe as Maybe
import Data.Tuple (Tuple(..), fst)

import THISAPP.Document (DocumentText(..), DocToken, fromString, documentText)
import THISAPP.Document.Position (
  rangeText, mkRange, documentRef
)
import THISAPP.Document.State (activeDoc)

import THISAPP.Parser (
  Parser, ErrorMsg(..), Message(..), HighlightColour(..), 
  addMessage, failHere, liftDocState, pposition
)
import THISAPP.Parser.Combinators (
  paint,
  choice, except, maybe, between, 
  surrBy, sepBy, sepByR, sepByInclRanges
)
import THISAPP.Parser.Token (
  TextParser, ParserOf, 
  alpha, char, eod, pattern, opsymbol, number, 
  space, ws, ws_, linebreak
)

import THISAPP.Program.Ast (
  Node(..), DeclarableExpression(..), Declaration(..), Descriptor, Expression(..), FunctionCall(..), 
  Identifier(..), Operator(..), Program(..), Statement(..)
)
import THISAPP.Program.ProgramSym (
  class ProgramSym, idName, parseArgs,
  idReplaces, idsToReplace, functionReplaces, functionsToReplace
)


type Any t = forall s. (ProgramSym s) => t s

type SomeParser' t s = Parser s (Node t s)
type Parser' t = Any (SomeParser' t)

parenthetical :: forall s a. (ProgramSym s) => Parser s a -> Parser s a
parenthetical = between (paint COLOUR_MINOR $ char '(') (paint COLOUR_MINOR $ char ')')

bracketed :: forall s a. (ProgramSym s) => Parser s a -> Parser s a
bracketed = between (paint COLOUR_MINOR $ char '[') (paint COLOUR_MINOR $ char ']')


tokenToText :: forall s. (ProgramSym s) => DocToken s -> DocumentText s
tokenToText = DocumentText <<< pure

equalsSign :: Any TextParser
equalsSign = tokenToText <$> char '='

specialSymbols :: Any TextParser
specialSymbols = equalsSign

parseOneOf :: forall s. (ProgramSym s) => Array (DocumentText s) -> TextParser s
parseOneOf = choice <<< map pattern

idToReplace :: Any TextParser
idToReplace = parseOneOf idsToReplace

functionToReplace :: Any TextParser
functionToReplace = parseOneOf functionsToReplace

specialFunctionNames :: forall s. (ProgramSym s) => Array (DocumentText s)
specialFunctionNames = map fromString 
  [ "sin", "cos", "tan", "arcsin", "arccos", "arctan", "ln", "exp" ]

specialFunction :: Any TextParser
specialFunction = parseOneOf specialFunctionNames


program :: Parser' Program
program = Node <$> Program <$> { statements: _ } <$> (do
  _  <- ws_
  xs <- statement `sepBy` linebreak
  _  <- ws_
  _  <- eod
  pure xs
)


statement :: Parser' Statement
statement = (do
  decl  <- declaration
  descr <- maybe descriptor
  pure $ Node $ DeclarationStatement { declaration: decl, descriptor: descr }
) <|> (do
  expr  <- expression
  descr <- maybe descriptor
  pure $ Node $ ExpressionStatement { expression: expr, descriptor: descr }
)


descriptor :: Parser' Descriptor
descriptor = empty -- @TODO: parse descriptors


no_op_expression :: Parser' Expression
no_op_expression = 
      Node <$> IdentifierExpression   <$> identifier COLOUR_VARIABLE
  <|> Node <$> NumberExpression       <$> paint COLOUR_NUMBER number
  <|> Node <$> FunctionCallExpression <$> functioncall
  <|> defer (\_ -> parenthetical expression)

expression :: Parser' Expression
expression = 
      no_op_expression
  <|> (do
    x  <- Right <$> no_op_expression
    xs <- concat <$> some (do
      _  <- ws_
      op <- Left <$> operator
      _  <- ws_
      y  <- Right <$> no_op_expression
      pure $ [ op, y ]
    )
    pure $ Node $ BinaryOperatorList { items: x:xs }
  )


dot :: Any TextParser
dot = tokenToText <$> char '.'

operator :: Parser' Operator
operator = 
      (Node <$> Operator <$> { name: _ } <$> DocumentText <$>
        paint COLOUR_OPERATOR (some opsymbol `except` (specialSymbols <|> dot))
      )
  <|> (Node <$> Operator <$> { name: _ } <$> (do
      _  <- ws
      op <- paint COLOUR_OPERATOR dot
      _  <- ws
      pure op
    ))


justId :: forall s a b. Parser s a -> Parser s (b -> Parser s (Tuple Int a))
justId p = pure $ const $ (Tuple 0 <$> p)

identifier :: HighlightColour -> Parser' Identifier
identifier colour =
  -- Parse a single token for identifier,
  -- or a longer string that we can replace with a single token, eg pi -> π
      Node <$> OneTokenIdentifier <$> { name: _ } <$> (
        paint colour (alpha `except` idToReplace)
        <|> do
          pos0 <- gets pposition
          id   <- idToReplace
          pos1 <- gets pposition
          case lookup id idReplaces of
            Just replacement -> do
              addMessage $ MsgHighlight (mkRange pos0 pos1) $ colour
              addMessage $ MsgReplace   (mkRange pos0 pos1) $ Left replacement
              pure replacement
            Nothing -> failHere $ CustomError $ fromString 
              "Internal error: id to replace did not have a replacement"
      )
  -- If within parentheses, we can refer to operators as well, eg (+) or (^)
  <|> parenthetical (Node <$> OperatorIdentifier <$> { op: _ } <$> operator)
  -- Within brackets, we can parse a longer string for identifier, 
  -- eg [var] or [my loooong name].
  -- These shouldn't be replaced no matter what, so [pi] -/-> π
  <|> bracketed (paint colour do
    x  <- alpha
    xs <- (do
      xs <- many (alpha <|> space)
      b  <- alpha
      pure (xs `snoc` b)
    ) <|> pure []
    pure $ Node $ LongIdentifier $ { name: DocumentText (x:xs) })
  -- If it's a special case, parse a longer string without brackets, 
  -- eg "sin" or "tan"
  <|> Node <$> SpecialIdentifier <$> { name: _ } <$> paint colour specialFunction


comma :: Any (ParserOf Unit)
comma = paint COLOUR_MINOR $ void (char ',' `surrBy` ws_)

functioncall :: Parser' FunctionCall
functioncall = Node <$> FunctionCall <$> (
  -- Parse a normal function call
  do
    function <- identifier COLOUR_FUNCTION_CALL
    _ <- ws_
    args <- parenthetical (expression `sepByR` comma)
    pure { function: function, args: args }
  -- Parse something we can replace with a function, ie sqrt or root(3, 8)
  <|> do
    pos0 <- gets pposition
    name <- functionToReplace
    pos1 <- gets pposition
    
    mArgsAndRanges <- maybe $ parenthetical ((expression `sepByInclRanges` comma) <* maybe comma)
    mDoc <- liftDocState activeDoc
    let argTexts = case mDoc of
                     Just doc ->
                       Maybe.maybe [] 
                         (map (fst >>> rangeText (documentText doc))) 
                         mArgsAndRanges
                     Nothing -> []
    
    case lookup name functionReplaces of
      Just replacementFn -> do
        newToken <- replacementFn argTexts (documentRef pos1)
        let id = idName newToken
        addMessage $ MsgReplace (mkRange pos0 pos1) $ Left newToken
        args <- parseArgs newToken
        pure { function: Node $ SpecialIdentifier { name: id }, args: args }
      Nothing -> failHere $ CustomError $ fromString 
        "Internal error: function call to replace did not have a replacement"
    )


declaration :: Parser' Declaration
declaration = do
  decl <- declarableExpression
  ws_
  void $ paint COLOUR_EQUALS_SIGN equalsSign
  ws_
  expr <- expression
  pure $ Node $ Declaration { declared: decl, rhs: expr }


declarableExpression :: Parser' DeclarableExpression
declarableExpression = (do
  var <- identifier COLOUR_VARIABLE
  pure $ Node $ VariableDeclare var 
) <|> (do
  fc <- functioncall
  pure $ Node $ FunctionDeclare fc
)
