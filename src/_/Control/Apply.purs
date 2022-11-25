module THISAPP.Control.Apply where
  
import Data.Function (flip, ($))
import Control.Apply (class Apply, lift2)


reversedApply :: forall f a b. Apply f => f a -> f (a -> b) -> f b
reversedApply = lift2 $ flip ($)

infixl 5 reversedApply as <**>
