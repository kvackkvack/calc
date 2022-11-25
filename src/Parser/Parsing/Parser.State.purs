module THISAPP.Parser.State (
  module THISAPP.Parser.State,
  module THISAPP.Document.State
) where

import Prelude

import Control.Monad.State (
  class MonadState, State,
  evalState, runState, get, put, modify_
)
import Data.Array (snoc)
import Data.Either (Either)
import Data.String (joinWith)
import Data.Tuple (Tuple(..))

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import THISAPP.Document (DocToken, DocumentText)
import THISAPP.Document.Position (Range, Position)
import THISAPP.Document.State (DocumentState, position, remainingDoc, emptyDocState)


newtype ParseState s = ParseState {
  messages :: Array (Message s),
  docState :: DocumentState s
}

derive instance eqParseState :: (Eq s) => Eq (ParseState s)

instance showParseState :: (Show s) => Show (ParseState s) where
  show (ParseState { docState: documentState, messages: msgs }) =
    " @ " <> show (position documentState)
    <> ", remaining = \"" <> show (evalState remainingDoc documentState) <> "\""
    <> (joinWith "" $ map (("\n  --  " <> _) <<< show) msgs)


-- # Document state
docState :: forall s. ParseState s -> DocumentState s
docState (ParseState s) = s.docState

fromDocState :: forall s. DocumentState s -> ParseState s
fromDocState documentState = ParseState {
  messages: [],
  docState: documentState
}


liftDocState :: forall a m s. (MonadState (ParseState s) m)
  => State (DocumentState s) a -> m a
liftDocState docStater = do
  ParseState state <- get
  let Tuple result docState' = runState docStater state.docState
  put (ParseState state { docState = docState' })
  pure result

pposition :: forall s. ParseState s -> Position
pposition = position <<< docState


-- # Messages
messages :: forall s. ParseState s -> Array (Message s)
messages (ParseState s) = s.messages

addMessage :: forall m s. (MonadState (ParseState s) m) => Message s -> m Unit
addMessage msg = modify_ \(ParseState state@{ messages: msgs }) -> 
  ParseState $ state { messages = msgs `snoc` msg }
  
  
data Message s = MsgReplace   Range (Either (DocToken s) (DocumentText s))
               | MsgHighlight Range HighlightColour
derive instance eqMessage :: (Eq s) => Eq (Message s)

instance showMessage :: (Show s) => Show (Message s) where
  show (MsgReplace r t)   = "REPLACE   " <> show r <> " with " <> show t
  show (MsgHighlight r c) = "HIGHLIGHT " <> show r <> " with " <> show c

messageIsReplace :: forall s. Message s -> Boolean
messageIsReplace (MsgReplace _ _) = true
messageIsReplace _ = false

messageIsHighlight :: forall s. Message s -> Boolean
messageIsHighlight (MsgHighlight _ _) = true
messageIsHighlight _ = false

messageRange :: forall s. Message s -> Range
messageRange (MsgReplace r _)   = r
messageRange (MsgHighlight r _) = r


data HighlightColour
  = COLOUR_VARIABLE
  | COLOUR_FUNCTION_CALL
  | COLOUR_EQUALS_SIGN
  | COLOUR_OPERATOR
  | COLOUR_NUMBER
  | COLOUR_MINOR

derive instance eqHighlightColour      :: Eq HighlightColour
derive instance genericHighlightColour :: Generic HighlightColour _
instance showHighlightColour           :: Show HighlightColour where
  show = genericShow
