
module System.Console.Terminal.Common
  ( Window(..)
  ) where

-- | Terminal window width and height
data Window a = Window
  { height :: !a
  , width  :: !a
  } deriving (Show, Read)

instance Functor Window where
  fmap f (Window { height = h, width = w }) = Window { height = f h, width = f w }
