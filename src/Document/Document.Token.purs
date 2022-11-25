module THISAPP.Document.Token where

import Prelude

import Data.Array (elem, find, (!!))
import Data.String (length)
import Data.String.CodePoints (singleton)
import Data.String.CodeUnits (toCharArray)
import Data.Char.Unicode as Unicode
import Data.Tuple (fst, snd)
import Data.Maybe (Maybe(..), isJust, fromJust)
import Control.Alternative ((<|>))
import Partial.Unsafe (unsafePartial)

import THISAPP.Document (
  class DocSym, DocToken(..), fromChar,
  lowerUpperPairs, alphaSymbols, digitSymbols, 
  alphaNumericSymbols, eods, operatorSymbols
)


type Tokener s t = DocToken s -> t

type BoolTokener      = forall s. (DocSym s) => Tokener s Boolean
type LooseBoolTokener = forall s. Tokener s Boolean
type TokenMapper      = forall s. (DocSym s) => Tokener s (DocToken s)


type Handler = forall s t.
  (String -> t) -> (s -> t) -> Tokener s t

handle :: Handler
handle f g t =
  case t of 
    DocTokenChar   c -> f $ singleton c
    DocTokenSymbol s -> g s


extractBool :: Maybe Boolean -> Boolean
extractBool Nothing  = false
extractBool (Just x) = x

justChar :: String -> Maybe Char
justChar s =
  if length s > 1 then
    Nothing
  else
    (toCharArray s) !! 0

acceptCharWhere :: (Char -> Boolean) -> (String -> Boolean)
acceptCharWhere f s = extractBool $ f <$> justChar s
  

-- Return true for digits
isDigit :: BoolTokener
isDigit = handle
  (acceptCharWhere Unicode.isDigit)
  (_ `elem` digitSymbols)


-- Return true for whitespace
isWhitespace :: LooseBoolTokener
isWhitespace = handle
  (acceptCharWhere Unicode.isSpace)
  (pure false)

-- Return true for newlines
isNewline :: LooseBoolTokener
isNewline = handle
  (_ `elem` ["\n", "\r"])
  (pure false)

-- Return true for uppercase tokens
isUpper :: BoolTokener
isUpper = handle
  (acceptCharWhere Unicode.isUpper)
  (\t -> isJust $ find (\p -> snd p == t) lowerUpperPairs)

-- Return true for lowercase tokens
isLower :: BoolTokener
isLower = handle
  (acceptCharWhere Unicode.isLower)
  (\t -> isJust $ find (\p -> fst p == t) lowerUpperPairs)

-- For lowercase tokens, return that token but uppercase
-- For all else, return the same token
toUpper :: TokenMapper
toUpper x = unsafePartial $ fromJust $ 
  (handle
    (\s -> (fromChar <<< Unicode.toUpper) <$> justChar s)
    (\t -> (DocTokenSymbol <<< snd)       <$> find (\p -> fst p == t) lowerUpperPairs)
  ) x <|> Just x

-- For all uppercase tokens, return that token but lowercase
-- For all else, return the same token
toLower :: TokenMapper
toLower x = unsafePartial $ fromJust $
  (handle
    (\s -> (fromChar <<< Unicode.toLower) <$> justChar s)
    (\t -> (DocTokenSymbol <<< fst)       <$> find (\p -> snd p == t) lowerUpperPairs)
  ) x <|> Just x
  

-- | Return true for alphabetic tokens (ie letters)
isAlpha :: BoolTokener
isAlpha = handle
  (acceptCharWhere Unicode.isAlpha)
  (_ `elem` alphaSymbols)

-- | Return true for alphanumeric tokens (ie letters + digits + digit-looking things)
isAlphaNum :: BoolTokener
isAlphaNum = handle
  (acceptCharWhere Unicode.isAlphaNum)
  (_ `elem` alphaNumericSymbols)

-- | Return true for EODs (end of documents)
isEOD :: BoolTokener
isEOD = handle
  (pure false)
  (_ `elem` eods)


specialSymbols :: Array Char
specialSymbols = toCharArray "()[]{},;'\"`_:"

asciiSymbols :: Array Char
asciiSymbols = toCharArray "!#$%&*+./<=>?@\\^|-~" 

-- | Return true for operator symbols (symbols which can make up operators)
-- Logic is stolen from Haskell source
-- It seems like only ` is actually considered a symbol by Unicode, though.
isOperatorSymbol :: BoolTokener
isOperatorSymbol = handle
  (acceptCharWhere $ (_ `elem` asciiSymbols) || (Unicode.isSymbol && not (_ `elem` specialSymbols)))
  (_ `elem` operatorSymbols)
