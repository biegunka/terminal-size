module System.Console.Terminal.Windows(size, hSize) where

import System.Console.Terminal.Common

import System.Exit
import System.IO
import System.IO.Error (catchIOError)
import System.Process
import System.Win32.Console
  ( CONSOLE_SCREEN_BUFFER_INFO(srWindow)
  , SMALL_RECT(..)
  , getConsoleScreenBufferInfo
  )
import System.Win32.Types (HANDLE, withHandleToHANDLE)

size :: Integral n => IO (Maybe (Window n))
size = hSize stdout

hSize :: Integral n => Handle -> IO (Maybe (Window n))
hSize hdl =
    withHandleToHANDLE hdl nativeSize
    `catchIOError` \_ -> do
        -- Fallback to use for Cygwin or MSYS
        let stty = (shell "stty size") {
              std_in  = UseHandle hdl
            , std_out = CreatePipe
            , std_err = CreatePipe
            }
        (_, mbStdout, _, rStty) <- createProcess stty
        exStty <- waitForProcess rStty
        case exStty of
            ExitFailure _ -> return Nothing
            ExitSuccess ->
                maybe (return Nothing)
                      (\out -> do
                          sizeStr <- hGetContents out
                          let [r, c] = map read $ words sizeStr :: [Int]
                          return $ Just $ Window (fromIntegral r) (fromIntegral c)
                      )
                      mbStdout

nativeSize :: Integral n => HANDLE -> IO (Maybe (Window n))
nativeSize hdl = do
    rect <- srWindow <$> getConsoleScreenBufferInfo hdl
    return $ Just $ Window
        { height = fromIntegral (1 + bottomPos rect - topPos rect)
        , width = fromIntegral (1 + rightPos rect - leftPos rect)
        }
