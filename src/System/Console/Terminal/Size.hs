{-# LANGUAGE CPP #-}
{- |
Get terminal window height and width without ncurses dependency

Based on answer by Andreas Hammar at <http://stackoverflow.com/a/12807521/972985>
-}
module System.Console.Terminal.Size
  ( Window(..)
  , size
#if !defined(mingw32_HOST_OS)
  , fdSize
#endif
  , hSize
  ) where

import System.Console.Terminal.Common
#if defined(mingw32_HOST_OS)
import qualified System.Console.Terminal.Windows as Host
#else
import qualified System.Console.Terminal.Posix as Host
import System.Posix.Types(Fd)
#endif
import System.IO(Handle)


-- | Get terminal window width and height for @stdout@.
--
-- >>> import System.Console.Terminal.Size
-- >>> size
-- Just (Window {height = 60, width = 112})
size :: Integral n => IO (Maybe (Window n))
size = Host.size

#if !defined(mingw32_HOST_OS)
-- | /Not available on Windows:/
-- Get terminal window width and height for a specified file descriptor. If
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
fdSize = Host.fdSize
#endif

-- | Same as 'fdSize', but takes 'Handle' instead of 'Fd' (file descriptor).
--
-- Note that on Windows with shells that use the native console API (cmd.exe,
-- PowerShell) this works only for output handles like 'stdout' and 'stderr';
-- for input handles like 'stdin' it always returns 'Nothing'.
--
-- >>> import System.Console.Terminal.Size
-- >>> import System.IO
-- >>> hSize stdout
-- Just (Window {height = 56, width = 85})
hSize :: Integral n => Handle -> IO (Maybe (Window n))
hSize = Host.hSize
