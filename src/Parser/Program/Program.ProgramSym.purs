module THISAPP.Program.ProgramSym where
  
import Prelude

import Data.Map (Map, keys)
import Data.Set (toUnfoldable)

import THISAPP.Document (class DocSym, DocumentRef, DocumentText, DocToken)
import THISAPP.Parser (Parser)
import THISAPP.Program.Ast (Node, Expression)


type ReplaceMap t s     = Map (DocumentText s) (t s) 
type IdReplacer s       = DocToken s
type FunctionReplacer s = Array (DocumentText s) -> DocumentRef -> Parser s (DocToken s)

class (DocSym s) <= ProgramSym s where
  idName    :: DocToken s -> DocumentText s
  parseArgs :: DocToken s -> Parser s (Array (Node Expression s))
  
  idReplaces :: ReplaceMap IdReplacer s
  functionReplaces :: ReplaceMap FunctionReplacer s

idsToReplace :: forall s. (ProgramSym s) => Array (DocumentText s)
idsToReplace = toUnfoldable $ keys idReplaces

functionsToReplace :: forall s. (ProgramSym s) => Array (DocumentText s)
functionsToReplace = toUnfoldable $ keys functionReplaces
  
  
