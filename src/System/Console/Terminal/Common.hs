
module System.Console.Terminal.Common
  ( Window(..)
  ) where

import Control.Applicative
import Data.Foldable (Foldable(..))
import Data.Traversable (Traversable(..))
import Data.Monoid (mappend)

-- | Terminal window width and height
data Window a = Window
  { height :: !a
  , width  :: !a
  } deriving (Show, Eq, Read)

instance Functor Window where
  fmap f (Window { height = h, width = w }) = Window { height = f h, width = f w }

instance Foldable Window where
  foldMap f (Window { height = h, width = w }) = f h `mappend` f w

instance Traversable Window where
  traverse f (Window { height = h, width = w }) = Window <$> f h <*> f w
