module THISAPP.Parser (
  module THISAPP.Parser,
  module THISAPP.Parser.Error,
  module THISAPP.Parser.State
) where

import Prelude

import Control.Alt (class Alt)
import Control.Apply (lift2)
import Control.Lazy (class Lazy, defer)
import Control.Monad.Except (class MonadError, ExceptT(..), runExceptT)
import Control.Monad.Error.Class (class MonadThrow, throwError)
import Control.Monad.State.Trans (class MonadState, StateT(..), gets, runStateT)
import Control.MonadPlus (class Alternative, class MonadZero, class MonadPlus, class Plus)

import Data.Array (
  (!!), (:), filter, findIndex, foldr, length, modifyAt, 
  reverse, sortWith, takeWhile
)
import Data.Either (Either(..), either, isRight)
import Data.Newtype (class Newtype, wrap, unwrap)
import Data.Maybe (Maybe(..), fromJust)
import Data.String (joinWith)
import Data.Tuple (Tuple(..), fst, snd)
import Partial.Unsafe (unsafePartial)

import THISAPP.Document (class DocSym, DocumentRef, fromString)
import THISAPP.Document.Position (Position, Range, mkJustPos, mkRange, sameAtTopLevel)
import THISAPP.Document.State (DocumentState, enterD, exitD, position)

import THISAPP.Parser.Error (
  ParseError(..), ErrorMsg(..), errorsMatch, combineErrors
)
import THISAPP.Parser.State (
  ParseState(..), Message(..), HighlightColour(..),
  pposition, addMessage, docState, fromDocState, liftDocState
)


type ParseResult s a = Either (ParseError s) (ParseSuccess s a)

newtype Parser s a = Parser (ExceptT (ParseError s) (StateT (ParseState s) Array) (ParseSuccess s a))
derive instance newtypeParser :: Newtype (Parser s a) _

data ParseSuccess s a = ParsingRemains (Parser s a)
                      | ParsingDone    a

isDone :: forall s a. ParseSuccess s a -> Boolean
isDone (ParsingDone _) = true
isDone _               = false

instance showParseSuccess :: (Show s, Show a) => Show (ParseSuccess s a) where
  show (ParsingRemains _) = "<pending>"
  show (ParsingDone    v) = "<done " <> show v <> ">"


newtype ParseResultList s a = ParseResultList (Array (Tuple (ParseResult s a) (ParseState s)))
derive instance newtypeParseResultList :: Newtype (ParseResultList s a) _

successfulParseResults :: forall s a. ParseResultList s a -> Array (Tuple (ParseResult s a) (ParseState s))
successfulParseResults (ParseResultList rs) = 
    filter (fst >>> either (const false) (isDone)) rs

instance showParseResultList :: (DocSym s, Show a) => Show (ParseResultList s a) where
  show results@(ParseResultList rs) =
    (if length finished > 0 then
      (if length finished > 1 then "!!! AMBIGUOUS GRAMMAR !!!\n\n" else "")
      <> showResults finished
     else
       "PARSING FAILED\n\n" <> showResults rs
    )
    where finished = successfulParseResults results
          showResults = joinWith "\n\n" <<< map (\(Tuple result state) ->
            (case result of
              Right v -> show v
              Left  e -> "<error " <> show e <> ">"
            ) <> show state
          )


foldErrors :: forall s a. (DocSym s) => Parser s a -> Parser s a
foldErrors p = makeParser \s ->
  foldr (\a@(Tuple r s') as -> case r of
    Left err -> case findIndex ((either (errorsMatch err) (const false)) <<< fst) as of
      Just index -> unsafePartial $ fromJust $
        modifyAt index (\b@(Tuple r' s'') -> case r' of
          Left err' -> Tuple (Left $ combineErrors err err') s''
          Right _   -> b -- This won't happen, but PureScript doesn't know that
        ) as
      Nothing -> a:as
    Right _ -> a:as
  ) [] $ runParser s p


parse :: forall s a. (DocSym s) => Parser s a -> DocumentState s -> ParseResultList s a
parse p dstate = wrap case nonErrors of
    [] -> (case rs !! 0 of
      Nothing -> []
      Just firstError ->
        -- Leave all the errors with the same top-level position in the document
        -- as the first (ie furthest along, since they're sorted) error
        takeWhile (errorPos >>> sameAtTopLevel (errorPos firstError)) rs
      ) <> pending
    _ -> nonErrors
  where errorPos  = pposition <<< snd
        rs        = reverse $ sortWith errorPos $ 
                    runParser (fromDocState dstate) $ foldErrors p
        nonErrors = filter (fst >>> isRight) rs
        pending   = filter (fst >>> either (const false) (not isDone)) nonErrors


makeParser :: forall s a. (ParseState s -> Array (Tuple (ParseResult s a) (ParseState s))) -> Parser s a
makeParser = Parser <<< ExceptT <<< StateT

runParser :: forall s a. ParseState s -> Parser s a -> Array (Tuple (ParseResult s a) (ParseState s))
runParser s (Parser p) = runStateT (runExceptT p) s


instance lazyParser :: Lazy (Parser s a) where
  defer f = Parser (ExceptT (defer (runExceptT <<< unwrap <<< f)))

instance semigroupParser :: (Semigroup a) => Semigroup (Parser s a) where
  append = lift2 (<>)

instance monoidParser :: (Monoid a) => Monoid (Parser s a) where
  mempty = pure mempty


instance functorParser :: Functor (Parser s) where
  map f p = makeParser \state -> 
    map (\(Tuple r state') ->
      Tuple (case r of
        Right x -> Right (case x of
          ParsingRemains p' -> ParsingRemains $ map f p'
          ParsingDone    v  -> ParsingDone $ f v)
        Left err -> Left err) state'
    ) $ runParser state p

instance applyParser :: Apply (Parser s) where
  apply pf p = makeParser \state -> 
    runParser state pf >>= (\(Tuple r state') ->
      case r of
        Right x -> case x of
          ParsingRemains pf' -> [ Tuple (Right $ ParsingRemains (apply pf' p)) state' ]
          ParsingDone    f   -> runParser state' (f <$> p)
        Left err -> [ Tuple (Left err) state' ]
    )

instance applicativeParser :: Applicative (Parser s) where
  pure v = makeParser \state -> [ Tuple (Right $ ParsingDone v) state ]
  
instance bindParser :: Bind (Parser s) where
  bind p f = makeParser \state ->
    runParser state p >>= (\(Tuple r state') ->
      case r of
        Right x -> case x of
          ParsingRemains p' -> [ Tuple (Right $ ParsingRemains (bind p' f)) state' ]
          ParsingDone    v  -> runParser state' (f v)
        Left err -> [ Tuple (Left err) state' ]
    )
instance monadParser :: Monad (Parser s)

instance monadStateParser :: MonadState (ParseState s) (Parser s) where
  state f = makeParser $ f >>> mapFst (Right <<< ParsingDone) >>> pure
    where
      mapFst :: forall a b c. (a -> b) -> Tuple a c -> Tuple b c 
      mapFst g (Tuple a c) = Tuple (g a) c

instance monadThrowParser :: MonadThrow (ParseError s) (Parser s) where
  throwError = throwError >>> map ParsingDone >>> wrap

instance monadErrorParser :: (DocSym s) => MonadError (ParseError s) (Parser s) where
  catchError p f = makeParser \s ->
    runParser s p >>= (\a@(Tuple r s') -> 
      case r of
        Left err -> runParser s' (f err)
        Right _  -> [a]
    )

instance altParser :: Alt (Parser s) where
  alt p q = makeParser \state ->
    let as = runParser state p
        bs = runParser state q in
      as <> bs
      {- case filter (isRight <<< fst) bs of
        [] -> as
        _  -> as <> bs -}

instance plusParser :: Plus (Parser s) where
  empty = makeParser \_ -> []

instance alternativeParser :: Alternative (Parser s)
instance monadZeroParser   :: MonadZero (Parser s)
instance monadPlusParser   :: MonadPlus (Parser s)


-- # Positioning

enter :: forall s. DocumentRef -> Parser s Unit
enter = liftDocState <<< enterD

exit :: forall s. Parser s Unit
exit = do
  pos <- gets pposition
  success <- liftDocState exitD
  if success then
    pure unit
  else
    throwError $ ParseError (mkJustPos pos) $ 
      CustomError (fromString 
        "Internal error: Can't exit out of top-level document"
      )
  
inside :: forall s a. DocumentRef -> Parser s a -> Parser s a
inside ref p = do
  enter ref
  x <- p
  exit
  pure x


-- # Errors

failHere :: forall s a. ErrorMsg s -> Parser s a
failHere msg = 
  gets (pposition >>> mkJustPos) 
  >>= failAt msg

failToHere :: forall s a. ErrorMsg s -> Position -> Parser s a
failToHere msg from = 
  gets (pposition >>> mkRange from) 
  >>= failAt msg

failFromHere :: forall s a. ErrorMsg s -> Position -> Parser s a
failFromHere msg to = 
  gets (\(ParseState s) -> mkRange (position s.docState) to) 
  >>= failAt msg

failAt :: forall s a. ErrorMsg s -> Range -> Parser s a
failAt msg range = throwError $ ParseError range msg
