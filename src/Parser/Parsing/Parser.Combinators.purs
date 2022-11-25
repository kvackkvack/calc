module THISAPP.Parser.Combinators where

import Prelude

import Control.Monad.State (gets)
import Control.Plus (empty, (<|>))

import Data.Array (filter, many, (:))
import Data.Either (Either(..), isRight)
import Data.Foldable (class Foldable, foldl)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), fst)

import THISAPP.Document (class DocSym, DocumentText)
import THISAPP.Document.Position (
  Position(..), Range(..), RangeLocationData(..),
  mkJustPos, mkRange, mkRangeL
)
import THISAPP.Document.State (DocumentState(..), position)
import THISAPP.Parser (
  ErrorMsg(..), ParseError(..), ParseState(..), Parser, 
  addMessage, docState, 
  makeParser, runParser,
  HighlightColour, Message(..)
)


-- # Errors

-- | Modify all error messages that might appear from parser,
-- | leaving successes untouched
mapErr :: forall s a. (ParseError s -> ParseError s) -> Parser s a -> Parser s a
mapErr f p = makeParser \state ->
  map (\a@(Tuple r s) -> case r of
    Left err -> Tuple (Left $ f err) s
    _        -> a
  ) $ runParser state p
infixl 4 mapErr as <?$>

-- | `mapErr` with the arguments reversed
mapErrFlip :: forall s a. Parser s a -> (ParseError s -> ParseError s) -> Parser s a
mapErrFlip p f = f <?$> p
infixl 3 mapErrFlip as <?#>

-- | Edit only the error message if parser fails
editErrorMessage :: forall s a. Parser s a -> (ErrorMsg s -> ErrorMsg s) -> Parser s a
editErrorMessage p f = p <?#> (\(ParseError r msg) -> ParseError r $ f msg)
infixl 3 editErrorMessage as <?*>

-- | Replace error message if parser fails
withErrorMessage :: forall s a. Parser s a -> ErrorMsg s -> Parser s a
withErrorMessage p msg = p <?*> const msg
infixl 3 withErrorMessage as <?>
 
-- | Append error message if parser fails
prependErrorMessage :: forall s a. Parser s a -> DocumentText s -> Parser s a
prependErrorMessage p label = p <?*> (\err -> Label label err)
infixl 3 prependErrorMessage as <+?>

-- | Any errors will be relocated to the beginning, also affecting the state
failAtStart :: forall s a. Parser s a -> Parser s a
failAtStart p = makeParser \s0 ->
  let start = position $ docState s0 in
  runParser s0 p <#> 
    (\a@(Tuple r (ParseState s@{docState: DocumentState docState})) -> case r of
      Left (ParseError _ msg) -> 
        Tuple 
          (Left $ ParseError (mkJustPos start) msg) 
          (ParseState $ s { docState = DocumentState docState { 
            position = start 
          }})
      _ -> a)

-- | Any errors will span from the beginning to as far as we got
failFromStart :: forall s a. Parser s a -> Parser s a
failFromStart p = do
  Position start <- gets (position <<< docState)
  p <?#> (\(ParseError (Range { range: r }) msg) -> case r of
    (JustPos    pos) -> ParseError (mkRangeL start.doc start.pos pos) msg
    (PosRange _ pos) -> ParseError (mkRangeL start.doc start.pos pos) msg
  )


-- # State

-- | Run a parser and highlight the parsed text with a certain colour
paint :: forall s a. HighlightColour -> Parser s a -> Parser s a
paint colour p = do
  pos0 <- gets (position <<< docState)
  x    <- p
  pos1 <- gets (position <<< docState)
  addMessage $ MsgHighlight (mkRange pos0 pos1) colour
  pure x


-- # Other

-- | Parse something without changing state (ie affecting the stream position)
lookAhead :: forall s a. Parser s a -> Parser s a
lookAhead p = makeParser \state0 -> do
  Tuple result _ <- runParser state0 p
  pure $ Tuple result state0
  

-- | Parse something or don't, discarding the result either way
optional :: forall s a. Parser s a -> Parser s Unit
optional p = void p <|> pure unit

-- | If the parser fails, return identity
optionalModifier :: forall s a. Parser s (a -> a) -> Parser s (a -> a)
optionalModifier p = p <|> pure identity

-- | Parse something (returning `Just` the result) or don't (returning `Nothing`)
maybe :: forall s a. Parser s a -> Parser s (Maybe a)
maybe p = (Just <$> p) <|> pure Nothing

-- | Parse anything from the first argument except things that also match the second
except :: forall s a b. (DocSym s) => Parser s a -> Parser s b -> Parser s a
except p q = makeParser \state ->
  let as = runParser state p
      bs = runParser state q
  in case filter (isRight <<< fst) bs of
    [] -> as
    x  -> []

-- | Parse the first argument, then the third, then the second, keeping only the result of the third
-- | - essentially nestling the third argument inbetween the first and second
between :: forall s a b c. Parser s b -> Parser s c -> Parser s a -> Parser s a 
between q r p = q *> p <* r

-- | Parse the second argument surrounded on both sides by the first,
-- | keeping only the result of the second
surrBy :: forall s a b. Parser s a -> Parser s b -> Parser s a
surrBy p q = between q q p


sepBy :: forall s a b. (DocSym s) => Parser s a -> Parser s b -> Parser s (Array a)
sepBy p q = do
    x  <- p
    xs <- many $ q >>= pure p
    pure (x:xs)

sepByR :: forall s a b. (DocSym s) => Parser s a -> Parser s b -> Parser s (Array a)
sepByR p q = (p `sepBy` q) <* (maybe q)

sepByL :: forall s a b. (DocSym s) => Parser s a -> Parser s b -> Parser s (Array a)
sepByL p q = (maybe q) *> (p `sepBy` q)

sepByLR :: forall s a b. (DocSym s) => Parser s a -> Parser s b -> Parser s (Array a)
sepByLR p q = (p `sepBy` q) `surrBy` q


sepByInclRanges :: forall s a b. (DocSym s) => Parser s a -> Parser s b -> Parser s (Array (Tuple Range a))
sepByInclRanges p q = do
  pos0 <- gets (position <<< docState)
  x    <- p
  pos1 <- gets (position <<< docState)
  let range = mkRange pos0 pos1
      tuple = Tuple range x
  xs <- sepByInclRanges' p q
  pure ((Tuple range x) : xs)
  
sepByInclRanges' :: forall s a b. (DocSym s) => Parser s a -> Parser s b -> Parser s (Array (Tuple Range a))
sepByInclRanges' p q = (q *> (sepByInclRanges p q)) <|> pure []


choice :: forall f s a. (Foldable f) => (DocSym s) => f (Parser s a) -> Parser s a
choice = foldl (<|>) empty
