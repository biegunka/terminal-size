{- |
Get terminal window height and width without ncurses dependency

Only tested to work on GNU/Linux systems

Based on answer by Andreas Hammar at <http://stackoverflow.com/a/12807521/972985>
-}
module System.Console.Terminal.Size
  ( Window(..), size
  ) where

import Control.Exception (catch)
import Foreign
import Foreign.C.Error
import Foreign.C.Types
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ >= 706)
import Prelude hiding (catch)
#endif

#include <sys/ioctl.h>
#include <unistd.h>


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


-- Interesting part of @struct winsize@
data CWin = CWin { wsRow, wsCol :: CUShort }

instance Storable CWin where
  sizeOf _ = (#size struct winsize)
  alignment _ = (#alignment struct winsize)
  peek ptr = do
    row <- (#peek struct winsize, ws_row) ptr
    col <- (#peek struct winsize, ws_col) ptr
    return $ CWin row col
  poke ptr (CWin row col) = do
    (#poke struct winsize, ws_row) ptr row
    (#poke struct winsize, ws_col) ptr col


-- | Terminal window width and height
data Window a = Window
  { height :: !a
  , width  :: !a
  } deriving (Show, Read)

instance Functor Window where
  fmap f (Window { height = h, width = w }) = Window { height = f h, width = f w }


-- | Get terminal window width and height
--
-- >>> import System.Console.Terminal.Size
-- >>> size
-- Window {height = 60, width = 112}
size :: Integral n => IO (Maybe (Window n))
size = catch go handler
 where
  go = with (CWin 0 0) $ \ws -> do
    throwErrnoIfMinus1 "ioctl" $
      ioctl (#const STDOUT_FILENO) (#const TIOCGWINSZ) ws
    CWin row col <- peek ws
    return . Just $ Window (fromIntegral row) (fromIntegral col)

  handler :: (IOError -> IO (Maybe (Window h)))
  handler _ = return Nothing

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr CWin -> IO CInt
