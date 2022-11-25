module THISAPP.DocSymbol where

import Prelude

import Control.Plus ((<|>))

import Data.Array ((!!), concatMap)
import Data.Map (fromFoldable)
import Data.Maybe (fromMaybe)
import Data.Tuple (Tuple(..), fst, snd) 

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import THISAPP.Document.State (addDoc)
import THISAPP.Parser (liftDocState, inside)
import THISAPP.Parser.Token (eod)

import THISAPP.Document (
  DocumentRef, Document(..), DocumentText(..), DocToken(..),
  class DocSym, fromString, fromChar
)

import THISAPP.Program.Ast (Expression(..), Node(..))
import THISAPP.Program.Parser (
  expression
)
import THISAPP.Program.ProgramSym (class ProgramSym, FunctionReplacer)


-- L: Lower. U: Upper. O: Other scalar symbol. P: Symbol with children.
data DocSymbol
  = O_EOD
  | P_2_ratio DocumentRef DocumentRef
  | P_2_root  DocumentRef DocumentRef

derive instance genericSymbol :: Generic DocSymbol _

derive instance ordSymbol :: Ord DocSymbol

derive instance eqSymbol :: Eq DocSymbol

instance showSymbol :: Show DocSymbol where
  show t = genericShow t

type Common t = t DocSymbol


lowerUpperPairs' :: Array (Tuple DocSymbol DocSymbol)
lowerUpperPairs' = []

alphaSymbols' :: Array DocSymbol
alphaSymbols' = 
  concatMap (\t -> [fst t, snd t]) lowerUpperPairs'
  
digitSymbols' :: Array DocSymbol
digitSymbols' = []

instance commonDocSymbol :: DocSym DocSymbol where
  lowerUpperPairs = lowerUpperPairs'
  alphaSymbols    = alphaSymbols'
  digitSymbols    = digitSymbols'
  
  alphaNumericSymbols = 
      alphaSymbols' <> digitSymbols'
      
  operatorSymbols = []
  
  eods = [ O_EOD ]


s :: String -> Common DocumentText
s = fromString

c :: Char -> Common DocToken
c = fromChar

make2 :: (DocumentRef -> DocumentRef -> DocSymbol)
       -> Common DocumentText -> Common DocumentText 
       -> Common DocumentText -> Common DocumentText
       -> Common FunctionReplacer
make2 token nameA defaultA nameB defaultB args docRef = 
  do
    Document docA <- arg 0 nameA defaultA
    Document docB <- arg 1 nameB defaultB
    pure (DocTokenSymbol $ token docA.ref docB.ref)
  where makeDocChild name text =
          liftDocState $ addDoc 
            (text <> DocumentText [ DocTokenSymbol O_EOD ]) name docRef
        arg i name default = makeDocChild name (fromMaybe default (args !! i))


instance commonProgramSymbol :: ProgramSym DocSymbol where
  idReplaces = fromFoldable [
      Tuple (s "pi") $ c 'π'
  ]
  
  idName (DocTokenSymbol (P_2_root _ _)) = s "root"
  idName _ = s "undefined"
  
  parseArgs (DocTokenSymbol (P_2_root indexDoc radicandDoc)) = do
      index <- inside indexDoc $ expression <|> do
        eod
        pure (Node $ NumberExpression 2.0)
      radicand <- inside radicandDoc $ expression
      pure [ index, radicand ]
  parseArgs _ = pure []
  
  functionReplaces = fromFoldable [
        Tuple (s "sqrt") $ bindArgs [s "" ] $ makeRoot ""
      , Tuple (s "cbrt") $ bindArgs [s "3"] $ makeRoot ""
      , Tuple (s "root") $ makeRoot "2"
  ] where 
      makeRoot :: String -> Common FunctionReplacer
      makeRoot b = make2 P_2_root (s "index") (s b) (s "radical") (s "")
      bindArgs :: Array (Common DocumentText) -> Common FunctionReplacer -> Common FunctionReplacer
      bindArgs args f xs = f (args <> xs)
