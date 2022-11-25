module THISAPP.HTML.HTML where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.State (class MonadState, State, put, get, runState)

import Data.Array ((!!))
import Data.Const (Const)
import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd)

import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Halogen (RefLabel(..), liftEffect)
import Halogen as H
import Halogen.Aff (awaitBody)
import Halogen.HTML (ClassName(..))
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.HTML.HTMLTextAreaElement as TextArea
import Web.UIEvent.KeyboardEvent (code)

import THISAPP.DocSymbol (DocSymbol)
import THISAPP.Document (class DocSym)
import THISAPP.Document.Position (pos0)
import THISAPP.Document.State (DocumentState, emptyDocState, position, insertAtCursor, backspaceAtCursor, withPosition, firstRef)
import THISAPP.Document.Text (fromString)
import THISAPP.HTML.Render (renderParseState)
import THISAPP.Parser (ParseResultList(..), parse, successfulParseResults)
import THISAPP.Program.Parser (Program, Node, program)
import THISAPP.Program.ProgramSym (class ProgramSym)


-- # Actions
data Action 
  = Input
  | Backspace

handleAction :: Action -> H.HalogenM CommonAppState Action () Void Aff Unit

handleAction Input = do
  maybeElem <- H.getHTMLElementRef (H.RefLabel "textarea")
  traverse_ (\elem -> do
    input <- liftEffect $ TextArea.value elem
    liftDocState $ insertAtCursor (fromString input)
    liftEffect $ TextArea.setValue "" elem
    reparse
  ) (TextArea.fromHTMLElement =<< maybeElem)

handleAction Backspace = do
  liftDocState (backspaceAtCursor 1)
  reparse


-- # Parsing
reparse :: forall m s. (DocSym s) => (ProgramSym s)
        => (MonadState (AppState s) m)
        => m Unit
reparse = do
  state@{ docState: docState } <- get
  let globalPos0 = pos0 firstRef
      results    = parse program (withPosition globalPos0 docState)
  put $ state { parseResults = results }


-- # State
type AppState s = {
  docState     :: DocumentState s,
  parseResults :: ParseResultList s (Node Program s)
}
type CommonAppState = AppState DocSymbol

initialState :: forall s. AppState s
initialState = {
  docState:     emptyDocState (fromString ""),
  parseResults: ParseResultList []
}


liftDocState :: forall a m s. (MonadState (AppState s) m)
  => State (DocumentState s) a -> m a
liftDocState docStater = do
  state <- get
  let Tuple result docState' = runState docStater state.docState
  put $ state { docState = docState' }
  pure result


-- # Render document
render :: CommonAppState -> H.ComponentHTML Action () Aff
render { docState: docState, parseResults: parseResults } =
  let (ParseResultList rs) = parseResults
      srs = successfulParseResults parseResults
      -- These are sorted, so the first is the furthest-along
      mFirstResult = (srs !! 0) <|> (rs !! 0)
  in HH.div
       [ HP.class_ $ ClassName "editor" ]
       $ [ HH.textarea
         [ HP.ref $ RefLabel "textarea"
         , HE.onInput (\_ -> Just Input)
         , HE.onKeyDown (\event -> 
             if code event == "Backspace" then 
               Just Backspace 
             else Nothing)
         ]
       ] <> (case mFirstResult of
         Just firstResult -> 
           [ renderParseState (snd firstResult) ] 
         Nothing -> [ HH.p_ [ HH.text "EMPTY" ] ])


main :: Effect Unit
main = launchAff_ do
  body <- awaitBody
  runUI (H.mkComponent
    { initialState: const initialState
    , render: render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  ) unit body
