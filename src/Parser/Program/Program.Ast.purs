module THISAPP.Program.Ast where

import Prelude

import Data.Array (concatMap)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.String.Pattern (Pattern(..))
import Data.String.Common (joinWith, split)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

import THISAPP.Document (DocToken, DocumentText)


indentWith :: forall a. String -> (a -> String) -> Array a -> String
indentWith indentation f =
  joinWith "\n" <<< concatMap (f >>> split (Pattern "\n") >>> map (indentation <> _))
  

data Node l t = Node (l t)

instance sNode :: (Show (l t)) => Show (Node l t) where 
  show (Node x) = show x


data Program t = Program {
  statements :: Array (Node Statement t)
}

instance sProgram :: (Show t) => Show (Program t) where
  show (Program p) 
    =  "Program [\n"
    <> indentWith "    " show p.statements
    <> "\n]"


data Statement t
  = DeclarationStatement { 
    declaration :: Node Declaration t, 
    descriptor  :: Maybe (Node Descriptor t)
  }
  | ExpressionStatement {
    expression :: Node Expression t,
    descriptor :: Maybe (Node Descriptor t)
  }

instance sStatement :: (Show t) => Show (Statement t) where
  show (DeclarationStatement ds) =
    "{ " <> show ds.declaration <> " }" <> (
      case ds.descriptor of
        Nothing -> ""
        Just x  -> " " <> show x 
    )
  show (ExpressionStatement ds) =
    "( " <> show ds.expression <> " )" <> (
      case ds.descriptor of
        Nothing -> ""
        Just x  -> " " <> show x 
    )


data Descriptor t = Descriptor { 
  -- @TODO
}

derive instance gDescriptor :: Generic (Descriptor t) _

instance sDescriptor :: (Show t) => Show (Descriptor t) where show = genericShow


data Expression t
  = IdentifierExpression   (Node Identifier t)
  | NumberExpression       (Number)
  | FunctionCallExpression (Node FunctionCall t)
  | BinaryOperatorList {
    items :: Array (Either (Node Operator t) (Node Expression t))
  }

derive instance gExpression :: Generic (Expression t) _

instance sExpression :: (Show t) => Show (Expression t) where 
  show (IdentifierExpression ie) = show ie
  show (NumberExpression n) = "<num> " <> show n
  show (FunctionCallExpression fc) = show fc
  show (BinaryOperatorList l) =
       "<binary operator list> {\n"
    <> (indentWith "    " (\item -> case item of
      Left  x -> " | " <> show x
      Right x -> show x
    ) l.items)
    <> "\n}"


data Identifier t 
  = OneTokenIdentifier {
    name :: DocToken t
  }
  | LongIdentifier {
    name :: DocumentText t
  }
  | OperatorIdentifier {
    op :: Node Operator t
  }
  | SpecialIdentifier {
    name :: DocumentText t
  }

instance sIdentifier :: (Show t) => Show (Identifier t) where
    show (OneTokenIdentifier t)  = "<id> "  <> show t.name
    show (LongIdentifier     ts) = "<id> [" <> show ts.name <> "]"
    show (OperatorIdentifier o)  = "<id> (" <> show o.op    <> ")"
    show (SpecialIdentifier  ts) = "<id> "  <> show ts.name


data Operator t = Operator {
  name :: DocumentText t
}

instance sOperator :: (Show t) => Show (Operator t) where
  show (Operator o) = "<op> " <> show o.name


data FunctionCall t
  = FunctionCall {
    function :: Node Identifier t,
    args     :: Array (Node Expression t)
  }

instance sFunctionCall :: (Show t) => Show (FunctionCall t) where
  show (FunctionCall fc) =
    "\\call (" <> show fc.function <> ") w/ (" <> (joinWith ", " $ map show fc.args) <> ")"


data Declaration t
  = Declaration {
    declared :: Node DeclarableExpression t,
    rhs      :: Node Expression t
  }
  
instance sDeclaration :: (Show t) => Show (Declaration t) where
  show (Declaration d) = "\\let (" <> show d.declared <> ") = " <> show d.rhs
  

data DeclarableExpression t
  = VariableDeclare (Node Identifier t)
  | FunctionDeclare (Node FunctionCall t)

derive instance gDeclarableExpression :: Generic (DeclarableExpression t) _

instance sDeclarableExpression :: (Show t) => Show (DeclarableExpression t) where
  show (VariableDeclare v) = show v
  show (FunctionDeclare f) = show f
