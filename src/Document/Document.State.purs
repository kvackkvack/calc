module THISAPP.Document.State where

import Prelude

import Control.Monad.State (class MonadState, get, gets, put, modify_)
import Data.Array (drop, length, take, (!!))
import Data.Map (Map, lookup, insert, empty, findMax)
import Data.Maybe (Maybe(..), maybe)
import Data.Newtype (class Newtype)

import THISAPP.Document (
  DocToken, Document(..), DocumentRef(..), DocumentText(..), 
  documentText, fromString
)
import THISAPP.Document.Position (
  Position(..), LocationData(..), LocationDataRecord, 
  advanceBy, ate, backtrack, documentRef, pos0, parent, spawn
)


newtype DocumentState s = DocumentState {
  position  :: Position,
  documents :: Map DocumentRef (Document s) 
}

derive instance newtypeDocumentState :: Newtype (DocumentState s) _
derive instance eqDocumentState :: (Eq s) => Eq (DocumentState s)
derive newtype instance showDocumentState :: (Show s) => Show (DocumentState s)

position :: forall s. DocumentState s -> Position
position (DocumentState d) = d.position 

documents :: forall s. DocumentState s -> Map DocumentRef (Document s)
documents (DocumentState d) = d.documents


firstRef :: DocumentRef
firstRef = DocumentRef 0

firstFreeRef :: forall s. DocumentState s -> DocumentRef
firstFreeRef (DocumentState { documents: docs }) = 
  maybe firstRef 
        (\max -> let DocumentRef n = max.key in DocumentRef (n + 1)) 
        (findMax docs)

addDoc :: forall m s. (MonadState (DocumentState s) m) =>
  DocumentText s -> DocumentText s -> DocumentRef -> m (Document s)
addDoc text name parent = do
  state@(DocumentState docState@({ documents: docs })) <- get
  let ref = firstFreeRef state
  let doc = Document {
    text:   text,
    name:   name,
    ref:    ref,
    parent: Just parent
  }
  put (DocumentState docState { documents = insert ref doc docs })
  pure doc

emptyDocState :: forall s. DocumentText s -> DocumentState s
emptyDocState text = 
  let ref = firstRef
  in DocumentState $ {
    position:  pos0 ref,
    documents: insert ref (Document {
      text:   text,
      name:   fromString "root",
      ref:    ref,
      parent: Nothing
    }) empty
  }

withPosition :: forall s. Position -> DocumentState s -> DocumentState s
withPosition pos' (DocumentState state@{ position: pos }) = 
  DocumentState state { position = pos' }

getDoc :: forall m s. (MonadState (DocumentState s) m)
          => DocumentRef -> m (Maybe (Document s))
getDoc ref = do
  DocumentState state <- get
  pure $ lookup ref state.documents

getOutermostDoc :: forall m s. (MonadState (DocumentState s) m)
          => m (Maybe (Document s))
getOutermostDoc = getDoc firstRef

activeDoc :: forall m s. (MonadState (DocumentState s) m)
          => m (Maybe (Document s))
activeDoc = do
  DocumentState state <- get
  getDoc (documentRef state.position)


withDoc :: forall m s t. (MonadState (DocumentState s) m)
  => DocumentRef -> (Document s -> m t) -> m t -> m t
withDoc ref f default = do
  maybeActiveDoc <- getDoc ref
  case maybeActiveDoc of
    Just doc -> f doc
    Nothing  -> default

withActiveDoc :: forall m s t. (MonadState (DocumentState s) m)
  => (Document s -> m t) -> m t -> m t
withActiveDoc f default = do
  DocumentState state <- get
  withDoc (documentRef state.position) f default

withLocationAndActiveDoc :: forall m s t. (MonadState (DocumentState s) m)
  => (LocationDataRecord -> Document s -> m t) -> m t -> m t
withLocationAndActiveDoc f default = withActiveDoc (\doc -> do
  DocumentState { position: Position { pos: LocationData location } } <- get
  f location doc
) default


updateDoc :: forall m s. (MonadState (DocumentState s) m)
          => DocumentRef -> (DocumentText s -> DocumentText s) -> m Boolean
updateDoc ref updater = withDoc ref (\(Document doc) -> do
    DocumentState state <- get
    let text' = updater doc.text
        doc'  = Document doc { text = text' }
        docs' = insert ref doc' state.documents
    put $ DocumentState state { documents = docs' }
    state' <- get
    pure true) 
  (pure false)

docInsertAt :: forall m s. (MonadState (DocumentState s) m)
            => Position -> DocumentText s -> m Position
docInsertAt pos@(Position p) text@(DocumentText ts) = do
  let i = ate pos
  success <- updateDoc (documentRef pos)
    (\(DocumentText vs) -> DocumentText $ take i vs <> ts <> drop i vs)
  if success then
    pure $ Position p { pos = advanceBy p.pos text }
  else
    pure pos

docBackspaceAt :: forall m s. (MonadState (DocumentState s) m)
               => Position -> Int -> m Position
docBackspaceAt pos@(Position p@{ pos: location }) n = do
  let i = ate pos
  
  pos' <- withDoc (documentRef pos) (\doc -> do
    pure $ Position p { pos = backtrack n location (documentText doc) }
  ) (pure pos)
  
  success <- updateDoc (documentRef pos)
    (\(DocumentText ts) -> DocumentText $ take (i - n) ts <> drop i ts)
  if success then 
    pure pos'
  else
    pure pos


atCursor :: forall a m s. (MonadState (DocumentState s) m)
         => (Position -> a -> m Position) -> a -> m Unit
atCursor f x = do
  pos  <- gets position
  pos' <- f pos x
  -- We need to get state *after* f since it (might) modify state 
  DocumentState state <- get
  put $ DocumentState state { position = pos' }

insertAtCursor :: forall m s. (MonadState (DocumentState s) m)
               => DocumentText s -> m Unit
insertAtCursor = atCursor docInsertAt

backspaceAtCursor :: forall m s. (MonadState (DocumentState s) m)
                  => Int -> m Unit
backspaceAtCursor = atCursor docBackspaceAt


remainingDoc :: forall m s. (MonadState (DocumentState s) m) 
             => m (DocumentText s)
remainingDoc = withLocationAndActiveDoc
  (\location doc -> pure $
    let DocumentText text = documentText doc
    in  DocumentText (drop location.ate text)
  ) (pure $ fromString "")

isAtEnd :: forall m s. (MonadState (DocumentState s) m) 
        => m Boolean
isAtEnd = withLocationAndActiveDoc
  (\location doc -> pure $
    let DocumentText text = documentText doc
    in  location.ate >= length text
  ) (pure false)


eat :: forall s m. MonadState (DocumentState s) m 
    => m (Maybe (DocToken s))
eat = do
  DocumentState state@{position: p@(Position pos@{ pos: location })} <- get
  maybeActiveDoc <- activeDoc
  case maybeActiveDoc of
    Nothing  -> pure Nothing
    Just doc -> do
      let DocumentText ts = documentText doc
          t = ts !! ate p 
      case t of
        Nothing -> pure unit
        Just t' -> do
          put $ DocumentState state { position = Position pos {
            pos = advanceBy location (DocumentText [ t' ])
          } }
      pure t


enterD :: forall s m. MonadState (DocumentState s) m
      => DocumentRef -> m Unit
enterD ref = modify_ \(DocumentState state) ->
  DocumentState $ state {
    position = spawn state.position ref
  }

exitD :: forall s m. MonadState (DocumentState s) m
      => m Boolean
exitD = do
  DocumentState state <- get
  let pos = state.position
  case parent pos of
    Just parentPos -> do
      put (DocumentState $ state { 
        position = parentPos 
      })
      pure true
    Nothing -> pure false
