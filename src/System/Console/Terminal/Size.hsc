{- |
Get terminal window height and width without ncurses dependency

Only tested to work on GNU/Linux systems

Based on answer by Andreas Hammar at <http://stackoverflow.com/a/12807521/972985>
-}
module System.Console.Terminal.Size
  ( Window(..), size, fdSize, hSize
  ) where

import Control.Exception (catch)
import Data.Typeable (cast)
import Foreign
import Foreign.C.Error
import Foreign.C.Types
import GHC.IO.FD (FD(FD, fdFD))
import GHC.IO.Handle.Internals (withHandle_)
import GHC.IO.Handle.Types (Handle, Handle__(Handle__, haDevice))
#if defined(__GLASGOW_HASKELL__) && (__GLASGOW_HASKELL__ < 706)
import Prelude hiding (catch)
#endif
import System.Posix.Types (Fd(Fd))

#include <sys/ioctl.h>
#include <unistd.h>


#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)


-- Interesting part of @struct winsize@
data CWin = CWin CUShort CUShort

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

-- | Get terminal window width and height for a specified file descriptor. If
-- it's not attached to a terminal then 'Nothing' is returned.
--
-- >>> import System.Console.Terminal.Size
-- >>> import System.Posix
-- >>> fdSize stdOutput
-- Just (Window {height = 56, width = 85})
-- >>> fd <- openFd "foo" ReadWrite (Just stdFileMode) defaultFileFlags
-- >>> fdSize fd
-- Nothing
fdSize :: Integral n => Fd -> IO (Maybe (Window n))
fdSize (Fd fd) = with (CWin 0 0) $ \ws -> do
  throwErrnoIfMinus1 "ioctl" $
    ioctl fd (#const TIOCGWINSZ) ws
  CWin row col <- peek ws
  return . Just $ Window (fromIntegral row) (fromIntegral col)
 `catch` handler
 where
  handler :: (IOError -> IO (Maybe (Window h)))
  handler _ = return Nothing

foreign import ccall "sys/ioctl.h ioctl"
  ioctl :: CInt -> CInt -> Ptr CWin -> IO CInt

-- | Get terminal window width and height for @stdout@.
--
-- >>> import System.Console.Terminal.Size
-- >>> size
-- Just (Window {height = 60, width = 112})
size :: Integral n => IO (Maybe (Window n))
size = fdSize (Fd (#const STDOUT_FILENO))

-- | Same as 'fdSize', but takes 'Handle' instead of 'Fd' (file descriptor).
--
-- >>> import System.Console.Terminal.Size
-- >>> import System.IO
-- >>> hSize stdout
-- Just (Window {height = 56, width = 85})
hSize :: Integral n => Handle -> IO (Maybe (Window n))
hSize h = withHandle_ "hSize" h $ \ Handle__{haDevice = dev} ->
    case cast dev of
        Nothing -> return Nothing
        Just FD{fdFD = fd} -> fdSize (Fd fd)
