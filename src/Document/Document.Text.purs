module THISAPP.Document.Text where

import Prelude

import Data.Array (dropEnd, foldl, length)
import Data.Tuple (Tuple)
import Data.String (CodePoint, toCodePointArray, joinWith, singleton)
import Data.String.CodePoints (codePointFromChar)
import Data.Newtype (class Newtype, unwrap, wrap)
import Data.Map (Map)


newtype DocumentText s = DocumentText (Array (DocToken s))

derive instance newtypeDocText :: Newtype (DocumentText s) _

derive newtype instance eqDocText :: (Eq s) => Eq (DocumentText s)

derive newtype instance ordDocText :: (Ord s) => Ord (DocumentText s)

derive newtype instance semigroupDocText :: Semigroup (DocumentText s)

instance showDocText :: (Show s) => Show (DocumentText s) where
  show (DocumentText ts) = joinWith "" $ map show ts

fromString :: forall s. String -> DocumentText s
fromString str = wrap $ map DocTokenChar $ toCodePointArray str


likeArrayDo :: forall a r. (Array (DocToken a) -> r) -> DocumentText a -> r
likeArrayDo f = f <<< unwrap

likeArrayMap :: forall a b. (Array (DocToken a) -> Array (DocToken b)) -> DocumentText a -> DocumentText b
likeArrayMap f = wrap <<< f <<< unwrap

textLength :: forall s. DocumentText s -> Int
textLength = likeArrayDo length

joinTextsWith :: forall s. DocumentText s -> Array (DocumentText s) -> DocumentText s
joinTextsWith join texts =
  likeArrayMap (dropEnd $ textLength join) $ foldl (\a b -> a <> b <> join) (fromString "") texts
  
  

data DocToken s
  = DocTokenChar   CodePoint
  | DocTokenSymbol s

derive instance eqToken :: (Eq s) => Eq (DocToken s)

derive instance ordToken :: (Ord s) => Ord (DocToken s)

instance showDocToken :: (Show s) => Show (DocToken s) where
  show (DocTokenChar   cp) = singleton cp
  show (DocTokenSymbol s)  = "$<" <> show s <> ">"

fromChar :: forall s. Char -> DocToken s
fromChar = DocTokenChar <<< codePointFromChar
