module THISAPP.Parser.Error where
  
import Prelude

import Data.Array (concat, fromFoldable, init, last, length, union)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..))

import THISAPP.Document (class DocSym, DocumentText, fromString, joinTextsWith)
import THISAPP.Document.Position (Range)


data ParseError s = ParseError Range (ErrorMsg s)

derive instance eqParseError :: (Eq s) => Eq (ParseError s)

instance showParseError :: (DocSym s) => Show (ParseError s) where
  show (ParseError range msg) = "\"" <> show msg <> "\" at " <> show range


data ErrorMsg s = Label       (DocumentText s) (ErrorMsg s)
                | Unexpect    (DocumentText s) (Array (DocumentText s))
                | CustomError (DocumentText s)

derive instance eqErrorMsg :: (Eq s) => Eq (ErrorMsg s)

displayErrorMsg :: forall s. (DocSym s) => ErrorMsg s -> DocumentText s
displayErrorMsg (Label label err) =
  label <> fromString ": " <> displayErrorMsg err
displayErrorMsg (Unexpect got expected) = case expected of
  []  -> (fromString "Unexpected ") <> got
  [x] -> (fromString "Unexpected ") <> got <> (fromString ", expected ") <> x
  xs  -> (fromString "Unexpected ") <> got <> (fromString ", expected ")
    <> (fromString (if length xs == 2 then "either " else "one of "))
    <> (joinTextsWith (fromString ", ") (concat $ fromFoldable $ init xs))
    <> (fromString " or ") <> (fromMaybe (fromString "") $ last xs) 
displayErrorMsg (CustomError txt) = txt

instance showErrorMsg :: (DocSym s) => Show (ErrorMsg s) where
  show = show <<< displayErrorMsg


errorsMatch :: forall s. (DocSym s) => ParseError s -> ParseError s -> Boolean
errorsMatch (ParseError r1 m1) (ParseError r2 m2)
  | r1 == r2  = errorMessagesMatch m1 m2
  | otherwise = false

errorMessagesMatch :: forall s. (DocSym s) => ErrorMsg s -> ErrorMsg s -> Boolean
errorMessagesMatch (Label t1 m1) (Label t2 m2) =
  t1 == t2 && errorMessagesMatch m1 m2
errorMessagesMatch (Unexpect got1 exp1) (Unexpect got2 exp2) =
  got1 == got2
errorMessagesMatch _ _ = false

combineErrors :: forall s. (DocSym s) => ParseError s -> ParseError s -> ParseError s
combineErrors (ParseError r1 m1) b@(ParseError r2 m2)
  | r1 == r2  = ParseError r2 $ combineErrorMessages m1 m2
  | otherwise = b

combineErrorMessages :: forall s. (DocSym s) => ErrorMsg s -> ErrorMsg s -> ErrorMsg s
combineErrorMessages a b
  | errorMessagesMatch a b = case Tuple a b of
    Tuple (Label t1 m1) (Label t2 m2) ->
      Label t2 $ combineErrorMessages m1 m2
    Tuple (Unexpect got1 exp1) (Unexpect got2 exp2) ->
      Unexpect got2 $ union exp1 exp2
    Tuple _ _ -> b
  | otherwise = b
