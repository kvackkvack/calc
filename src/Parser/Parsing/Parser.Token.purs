module THISAPP.Parser.Token where

import Prelude

import Control.Monad.State (runState)
import Control.Plus ((<|>))
import THISAPP.Control.Apply ((<**>))

import Data.Array (tail, some, many, elem, (:), (!!))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.String (joinWith)
import Data.Ord (signum)
import Data.Int as Int
import Data.Number as Number

import THISAPP.Document (class DocSym, DocToken(..), DocumentText(..), fromString, fromChar)
import THISAPP.Document.Token as Token
import THISAPP.Document.State (eat, isAtEnd)
import THISAPP.Parser (
  Parser, ParseState(..), ParseSuccess(..), ErrorMsg(..), 
  failHere, liftDocState, makeParser
)
import THISAPP.Parser.Combinators (
  (<+?>), (<?*>), failAtStart, failFromStart,
  optionalModifier, except, sepByLR
)


-- # Basic types

type Any t = forall s. (DocSym s) => t s

type TokenParser s = Parser s (DocToken s)
type TextParser  s = Parser s (DocumentText s)

type ParserOf a s = Parser s a 


name_eod :: Any DocumentText
name_eod = fromString "end of document"

showToken :: forall s. (DocSym s) => DocToken s -> DocumentText s
showToken t = DocumentText [ fromChar '\'', t, fromChar '\'' ]

showText :: forall s. (DocSym s) => DocumentText s -> DocumentText s
showText txt = fromString "\"" <> txt <> fromString "\""

unexpectedToken :: forall s. (DocSym s) => DocToken s -> Array (DocumentText s) -> ErrorMsg s
unexpectedToken t e = Unexpect ((fromString "token ") <> showToken t) e


-- | The unit parser. Eats any token.
item' :: Any TokenParser
item' = makeParser \(ParseState state@{ docState: dstate }) ->
  let Tuple t dstate' = runState eat dstate
      state' = ParseState $ state { docState = dstate' } 
  in [ Tuple (Right $ (case t of
    Nothing -> ParsingRemains item
    Just t' -> ParsingDone t')) state' ]

-- | Eats any token except EOF.
item :: Any TokenParser
item = failAtStart do
  t <- item'
  if Token.isEOD t then
    failHere $ Unexpect name_eod []
  else
    pure t

-- | Eats EOD or succeeds anyway if the end of the document was reached.
eod :: Any (ParserOf Unit)
eod = failAtStart do
  reachedEnd <- liftDocState isAtEnd
  if reachedEnd then
    pure unit
  else do
    t <- item'
    if Token.isEOD t then
      pure unit
    else
      failHere $ unexpectedToken t [ name_eod ]


-- # Primitive parser creators (functions from non-parsers to parsers)

satisfyWithLabels :: forall s. (DocSym s) => Array (DocumentText s) -> (DocToken s -> Boolean) -> TokenParser s
satisfyWithLabels labels p = failAtStart do
  x <- item
  if p x then
    pure x
  else
    failHere $ unexpectedToken x labels

-- | Eat any token satisfying the given predicate
-- | The first argument labels the allowed tokens, eg. "alphanumeric character" or "digit"
-- | The error will look like "Unexpected token <received token>, expected <label>"
satisfyWithLabel :: forall s. (DocSym s) => DocumentText s -> (DocToken s -> Boolean) -> TokenParser s
satisfyWithLabel str = satisfyWithLabels [str]

-- | Labelless satisfier
satisfy :: forall s. (DocSym s) => (DocToken s -> Boolean) -> TokenParser s
satisfy = satisfyWithLabels []


-- | Eat single certain token
token :: forall s. (DocSym s) => DocToken s -> TokenParser s
token t = satisfyWithLabel (showToken t) (_ == t)

-- | Eat a single one of the allowed tokens
oneOf :: forall s. (DocSym s) => Array (DocToken s) -> TokenParser s
oneOf ts = satisfyWithLabels (map showToken ts) (_ `elem` ts)


-- | Eat single certain character
char :: forall s. (DocSym s) => Char -> TokenParser s
char = token <<< fromChar

-- | Eat single certain symbol
symbol :: forall s. (DocSym s) => s -> TokenParser s
symbol = token <<< DocTokenSymbol


-- | Allow only exact matches of pattern
pattern :: forall s. (DocSym s) => DocumentText s -> TextParser s
pattern text@(DocumentText ts) = 
  let mx  = ts !! 0
      mxs = tail ts
  in case mx of
    Just x -> case mxs of
      Just xs ->
        failFromStart (do 
           _ <- token x <?*> (\msg -> case msg of
             Unexpect got _ -> Unexpect got [ showText text ]
             other -> other
           ) 
           _ <- pattern $ DocumentText xs
           pure $ DocumentText (x:xs))
      Nothing -> pure $ DocumentText []
    Nothing -> pure $ DocumentText[]


-- # Primitive parsers

-- | Parse a single digit
digit :: Any TokenParser
digit = satisfyWithLabel (fromString "digit (0-9)") Token.isDigit

-- | Parse a single alphabetic token
alpha :: Any TokenParser
alpha = satisfyWithLabel (fromString "alphabetic character") Token.isAlpha

-- | Parse a single alphanumeric token
alphaNum :: Any TokenParser
alphaNum = satisfyWithLabel (fromString "alphanumeric character") Token.isAlphaNum

-- | Parse a single uppercase token
upper :: Any TokenParser
upper = satisfyWithLabel (fromString "uppercase character") Token.isUpper

-- | Parse a single lowercase token
lower :: Any TokenParser
lower = satisfyWithLabel (fromString "lowercase character") Token.isLower

-- | Parse a single operator symbol
opsymbol :: Any TokenParser
opsymbol = satisfyWithLabel (fromString "legal operator symbol") Token.isOperatorSymbol


-- # Whitespace

-- | Parse a single newline character (\n or \r). Not very useful; try `newline` or `linebreak` instead
nl_char :: Any TokenParser
nl_char = oneOf [ fromChar '\n', fromChar '\r' ]
-- | Parse a newline, ie any amount of \n and \r
newline :: forall s. DocSym s => Parser s (Array (DocToken s))
newline = some nl_char
-- | Parse a single bit of whitespace, including newlines
space :: Any TokenParser
space = satisfyWithLabel (fromString "whitespace") Token.isWhitespace
-- | Parse at least one bits of whitespace
ws :: Any (ParserOf Unit)
ws = void $ some space
-- | Parse any amount (including 0) of whitespace
ws_ :: Any (ParserOf Unit)
ws_ = void $ many space
-- | Parse a single bit of non-newline whitespace
space' :: Any TokenParser
space' = space `except` nl_char
-- | Parse at least one bits of non-newline whitespace
ws' :: Any (ParserOf Unit)
ws' = void $ some space'
-- | Parse any amount (including 0) of non-newline whitespace
ws_' :: Any (ParserOf Unit)
ws_' = void $ many space'
-- | Parse a linebreak, ie a newline optionally surrounded by non-newline whitespace
linebreak :: forall s. DocSym s => Parser s (Array (DocToken s))
linebreak = nl_char `sepByLR` ws_


-- # Numbers

-- | Parse an unsigned integer (ie multiple digits)
uint :: Any (ParserOf Int)
uint = some digit <#> fromMaybe 0 <<< Int.fromString <<< joinWith "" <<< map show

-- | Parse a sign (+ or -), then result in a method which will apply that sign to a number
-- | eg. you could define int = optional signed <*> uint
sign :: forall a. (Ring a) => Any (ParserOf (a -> a))
sign = do
  sgn <- char '-' <|> char '+'
  if sgn == fromChar '-' then
    pure negate
  else
    pure identity

-- | Parse a "." and then some digits, then result in a method which will add that decimal part to another number
-- | eg. you could define float = int <**> decimals
decimals :: Any (ParserOf (Int -> Number))
decimals = do
  dot <- char '.'
  ds  <- uint
  let decimalPart = fromMaybe 0.0 $ Number.fromString $ (show dot) <> (show ds)
  pure $ (\n -> n + (signum n) * decimalPart) <<< Int.toNumber

-- | Parse an integer (ie an optional sign (+/-) and some digits)
int :: Any (ParserOf Int)
int = optionalModifier sign <*> uint

-- | Parse a floating point number (ie an optional sign (+/-), some optional digits, a ".", and some more digits)
float :: Any (ParserOf Number)
float = optionalModifier sign <*> (uint <|> pure 0) <**> decimals

-- | Parse an integer or a float
number :: Any (ParserOf Number)
number = (float <|> (int <#> Int.toNumber) 
  <+?> (fromString "While trying to parse number"))
