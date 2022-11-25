module THISAPP.Document.Position where

import Prelude

import Data.Array (length, reverse, slice, take, takeWhile, uncons, unsnoc, (!!))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Data.Tuple (Tuple(..))

import THISAPP.Document (DocumentRef(..), DocumentText(..), likeArrayMap)
import THISAPP.Document.Token (isNewline)


type DocumentDataRecord = {
  documentRef :: DocumentRef,
  parent      :: Maybe Position
}
newtype DocumentData = DocumentData DocumentDataRecord
derive instance newtypeDocumentData :: Newtype DocumentData _

derive instance eqDocumentData :: Eq DocumentData

instance showDocumentData :: Show DocumentData where
  show (DocumentData { documentRef: DocumentRef n }) = "#" <> show n


type LocationDataRecord = {
  ate    :: Int,
  line   :: Int, 
  column :: Int
}
newtype LocationData = LocationData LocationDataRecord
derive instance newtypeLocationData :: Newtype LocationData _

derive instance eqLocationData :: Eq LocationData

instance showLocationData :: Show LocationData where 
  show (LocationData { line: line, column: column }) = 
    show line <> ":" <> show column

instance ordLocationData :: Ord LocationData where
  compare (LocationData a) (LocationData b)
    = compare a.ate b.ate

location0 :: LocationData
location0 = LocationData { ate: 0, line: 1, column: 1 }


newtype Position = Position {
  doc :: DocumentData,
  pos :: LocationData
}
derive instance eqPosition :: Eq Position

instance showPosition :: Show Position where
  show (Position { doc: doc@(DocumentData d), pos: pos }) = (case d.parent of
    Nothing -> ""
    Just parentPos -> show parentPos <> ", "
  ) <> show pos <> "|" <> show doc


instance ordPosition :: Ord Position where
  compare a@(Position posA) b@(Position posB) = 
    case Tuple (parent a) (parent b) of
      Tuple (Just pA) mPB   -> compareParents pA (fromMaybe b mPB)
      Tuple mPA (Just pB)   -> compareParents (fromMaybe a mPA) pB
      Tuple Nothing Nothing -> compareThese
    where compareThese = compare posA.pos posB.pos
          compareParents a' b' = compare a' b' <> compareThese

sameAtTopLevel :: Position -> Position -> Boolean
sameAtTopLevel a b = 
  case Tuple (parent a) (parent b) of
    Tuple (Just pA) mPB   -> sameAtTopLevel pA (fromMaybe b mPB)
    Tuple mPA (Just pB)   -> sameAtTopLevel (fromMaybe a mPA) pB
    Tuple Nothing Nothing -> ate a == ate b


documentData :: Position -> DocumentData
documentData (Position p) = p.doc

documentRef :: Position -> DocumentRef
documentRef (Position p) = let DocumentData doc = p.doc in doc.documentRef

parent :: Position -> Maybe Position
parent (Position p) = let DocumentData doc = p.doc in doc.parent

ate :: Position -> Int
ate (Position p) = let LocationData l = p.pos in l.ate


makeChildOf :: Maybe Position -> DocumentRef -> Position
makeChildOf p docRef = Position {
  doc: DocumentData {
    documentRef: docRef,
    parent: p
  },
  pos: location0
}

spawn :: Position -> DocumentRef -> Position
spawn p = makeChildOf (Just p)

pos0 :: DocumentRef -> Position
pos0 = makeChildOf Nothing


advanceBy :: forall s. LocationData -> DocumentText s -> LocationData
advanceBy p@(LocationData pos) (DocumentText tts) =
  case uncons tts of
    Nothing -> p
    Just { head: t, tail: ts } ->
      let pos' = if isNewline t then
                   succLine pos
                 else
                   succChar pos
      in advanceBy (LocationData pos') (DocumentText ts)
  where
    succChar x = x { ate = x.ate + 1, column = x.column + 1 }
    succLine x = x { ate = x.ate + 1, line = x.line + 1, column = 1 }   

backtrack :: forall s. Int -> LocationData -> DocumentText s -> LocationData
backtrack 0 pos text = pos
backtrack n p@(LocationData pos) text@(DocumentText tts) =
  let eaten = take pos.ate tts
  in case unsnoc eaten of
    Nothing -> p -- We've reached the beginning of the document
                 -- so we can't backspace anymore
    Just { init: ts, last: t } ->
      let pos' = if isNewline t then
                   predLine ts pos
                 else
                   predChar pos
      in backtrack (n-1) (LocationData pos') text
  where
    predChar x = x { ate = x.ate - 1, column = x.column - 1 }
    predLine ts x =
      let lastLineLen = length $ takeWhile (not <<< isNewline) (reverse ts)
      in x { ate = x.ate - 1, line = x.line - 1, column = 1 + lastLineLen }


-- # Ranges
data RangeLocationData 
  = PosRange LocationData LocationData
  | JustPos  LocationData
derive instance eqRangeLocationData :: Eq RangeLocationData

instance ordRangeLocationData :: Ord RangeLocationData where
  compare (PosRange a1 b1) (PosRange a2 b2) = 
    compare a1 a2 <> compare b1 b2
  compare (JustPos a1) (JustPos a2) = compare a1 a2 
  compare (PosRange a1 b1) (JustPos a2) =
    compare a1 a2 <> compare b1 a2
  compare justPos range = compare range justPos

rangeSpanStart' :: RangeLocationData -> LocationData
rangeSpanStart' (PosRange x _) = x
rangeSpanStart' (JustPos x)    = x

rangeSpanEnd' :: RangeLocationData -> LocationData
rangeSpanEnd' (PosRange _ x) = x
rangeSpanEnd' (JustPos x)    = x


newtype Range = Range {
  doc   :: DocumentData,
  range :: RangeLocationData
}
derive instance eqRange :: Eq Range

rangeDocument :: Range -> DocumentData
rangeDocument (Range r) = r.doc

rangeParent :: Range -> Maybe Position
rangeParent (Range { doc: DocumentData doc }) = doc.parent

rangeSpanStart :: Range -> Position
rangeSpanStart (Range { doc: doc, range: r }) 
  = Position { doc: doc, pos: rangeSpanStart' r }

rangeSpanEnd :: Range -> Position
rangeSpanEnd (Range { doc: doc, range: r }) 
  = Position { doc: doc, pos: rangeSpanEnd' r }

instance ordRange :: Ord Range where
  compare a b =
    case Tuple (rangeParent a) (rangeParent b) of
      Tuple (Just pA) (Just pB) -> compare pA pB <> compareThese
      Tuple Nothing Nothing     -> compareThese
      Tuple Nothing (Just pB)   -> compare (rangeSpanStart a) pB <> compareThese
      Tuple (Just pA) Nothing   -> compare pA (rangeSpanStart b) <> compareThese
    where compareRanges (Range x) (Range y) = compare x.range y.range
          compareThese = compareRanges a b

instance showRange :: Show Range where
  show (Range { doc: doc, range: PosRange a b }) = 
    show a <> " - " <> show b <> "|" <> show doc
  show (Range { doc: doc, range: JustPos p }) = 
    show p <> "|" <> show doc


mkJustPos :: Position -> Range
mkJustPos (Position { doc: doc, pos: pos }) = 
  Range { doc: doc, range: JustPos pos }

mkRange :: Position -> Position -> Range
mkRange (Position { doc: doc1, pos: pos1 })
         (Position { doc: doc2, pos: pos2 })
  = Range { doc: doc1, range: PosRange pos1 pos2 }

mkRangeL :: DocumentData -> LocationData -> LocationData -> Range
mkRangeL doc pos1 pos2 = Range { doc: doc, range: PosRange pos1 pos2 }

rangeText' :: forall s. (Eq s) => DocumentText s -> RangeLocationData -> DocumentText s
rangeText' text (PosRange (LocationData a) (LocationData b)) = 
  likeArrayMap (slice a.ate b.ate) text
rangeText' (DocumentText ts) (JustPos (LocationData p)) = case ts !! p.ate of
  Just t  -> DocumentText [ t ]
  Nothing -> DocumentText []

rangeText :: forall s. (Eq s) => DocumentText s -> Range -> DocumentText s
rangeText text (Range r) = rangeText' text r.range
