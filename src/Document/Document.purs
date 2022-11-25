module THISAPP.Document (
  module THISAPP.Document,
  module THISAPP.Document.Text
) where

import Prelude

import Data.Tuple (Tuple)
import Data.Maybe (Maybe(..))

import THISAPP.Document.Text (
  DocToken(..), DocumentText(..), fromChar, fromString, 
  joinTextsWith, likeArrayDo, likeArrayMap, textLength
)


newtype DocumentRef = DocumentRef Int
derive newtype instance eqDocumentRef   :: Eq DocumentRef
derive newtype instance ordDocumentRef  :: Ord DocumentRef
derive newtype instance showDocumentRef :: Show DocumentRef 


newtype Document s = Document {
  text   :: DocumentText s,
  name   :: DocumentText s,
  ref    :: DocumentRef,
  parent :: Maybe DocumentRef
}

derive instance eqDocument :: (Eq s) => Eq (Document s)

instance ordDocument :: (Ord s) => Ord (Document s) where
  compare (Document a) (Document b) = compare a.text b.text

instance showDoc :: (Show s) => Show (Document s) where
  show (Document doc) = "{" <> show doc.name <> "}" <> " \"" <> show doc.text <> "\""

documentText :: forall s. Document s -> DocumentText s
documentText (Document d) = d.text

documentRef :: forall s. Document s -> DocumentRef
documentRef (Document d) = d.ref


class (Show s, Eq s, Ord s) <= DocSym s where
  lowerUpperPairs :: Array (Tuple s s)
  alphaSymbols :: Array s
  digitSymbols :: Array s
  alphaNumericSymbols :: Array s
  operatorSymbols :: Array s
  eods :: Array s
