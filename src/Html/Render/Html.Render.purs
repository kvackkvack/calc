module THISAPP.HTML.Render where

import Prelude

import Control.Monad.State (evalState)
import Data.Array ((:), concatMap, filter, last, mapMaybe, uncons)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (unwrap)
import Data.Tuple (Tuple(..))

import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import THISAPP.Document (Document, documentText, documentRef)
import THISAPP.Document.State (getOutermostDoc)
import THISAPP.Document.Position (
  LocationData, Range(..), RangeLocationData(..),
  advanceBy, location0, rangeSpanEnd', rangeSpanStart',
  rangeDocument, rangeText'
)
import THISAPP.Parser.State (
  Message(..), HighlightColour(..), ParseState(..), 
  messageRange
)


data LocalHighlight = LocalHighlight RangeLocationData (Maybe HighlightColour)

makeHighlightLocal :: forall s. Message s -> Maybe LocalHighlight
makeHighlightLocal (MsgHighlight (Range r) colour) = 
  Just $ LocalHighlight r.range (Just colour)
makeHighlightLocal _ = Nothing


renderParseState :: forall s w i. (Show s) => (Eq s) => ParseState s -> HH.HTML w i
renderParseState (ParseState { messages: msgs, docState: docState }) =
  case evalState getOutermostDoc docState of
    Just doc0 -> renderDocument doc0 msgs
    Nothing   -> HH.pre_ [ HH.text "error - no docs" ]


pairWithNext :: forall a. a -> a -> Array a -> Array (Tuple a a)
pairWithNext padStart padEnd xxs = case uncons xxs of
  Nothing -> 
    [Tuple padStart padEnd]
  Just { head: x, tail: xs } ->
    Tuple padStart x : pairWithNext x padEnd xs

-- @TODO: this could probably be done better...
fillOutHighlights :: LocationData -> LocationData 
                  -> Array LocalHighlight -> Array LocalHighlight
fillOutHighlights start end xs = case uncons xs of
  Nothing -> 
    [LocalHighlight (PosRange start end) Nothing]
  Just { head: x, tail: tail } ->
    let lst    = fromMaybe x (last tail)
        startH = LocalHighlight (JustPos start) Nothing
        endH   = LocalHighlight (JustPos end)   Nothing
        pairs  = pairWithNext startH endH xs
    in concatMap (\(Tuple 
         curr@(LocalHighlight currRange currColour) 
         (LocalHighlight nextRange nextColour)) -> 
           let currEnd   = rangeSpanEnd' currRange
               nextStart = rangeSpanStart' nextRange
           -- If currColour is Nothing, this can't be an *actual*
           -- highlight, ie it's our faked filler startH; no need
           -- to include that.
           -- If currEnd = nextStart, there's nothing between them
           -- that our highlights are missing, so no need to add
           -- that padding.
           in (if currColour == Nothing then []
               else [curr])
           <> (if currEnd == nextStart then []
               else [highlightNeutral currEnd nextStart])
        ) pairs
   where
     highlightNeutral :: LocationData -> LocationData -> LocalHighlight
     highlightNeutral a b = LocalHighlight (PosRange a b) Nothing


renderDocument :: forall s w i. (Show s) => (Eq s) 
               => Document s -> Array (Message s) -> HH.HTML w i
renderDocument doc msgs =
  HH.pre [HP.class_ $ ClassName "document"] 
    $ map (\(LocalHighlight range colour) -> 
      let highlightedText = rangeText' text range
          classes = map (HP.class_ <<< ClassName) (highlightClasses colour)
      in HH.span 
           classes
           [HH.text $ show highlightedText]
    ) filledHighlights
  where
    text  = documentText doc
    myRef = documentRef doc
    myMessages = filter 
      ((_ == myRef) <<< _.documentRef <<< unwrap <<< rangeDocument <<< messageRange) 
      msgs
    myHighlights = mapMaybe makeHighlightLocal myMessages
    posStart = location0
    posEnd = advanceBy posStart text
    filledHighlights = fillOutHighlights posStart posEnd myHighlights


highlightClasses :: Maybe HighlightColour -> Array String   
highlightClasses Nothing = []
highlightClasses (Just COLOUR_VARIABLE)      = ["col-variable"]
highlightClasses (Just COLOUR_FUNCTION_CALL) = ["col-function-call"]
highlightClasses (Just COLOUR_EQUALS_SIGN  ) = ["col-equals"]
highlightClasses (Just COLOUR_OPERATOR)      = ["col-operator"]
highlightClasses (Just COLOUR_NUMBER)        = ["col-number"]
highlightClasses (Just COLOUR_MINOR)         = ["col-minor"]
