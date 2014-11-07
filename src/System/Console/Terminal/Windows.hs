
module System.Console.Terminal.Windows(size) where

import System.Console.Terminal.Common

import Control.Monad
import Data.Word
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

type HANDLE = Ptr ()

data CONSOLE_SCREEN_BUFFER_INFO

sizeCONSOLE_SCREEN_BUFFER_INFO :: Int
sizeCONSOLE_SCREEN_BUFFER_INFO = 22

posCONSOLE_SCREEN_BUFFER_INFO_srWindow :: Int
posCONSOLE_SCREEN_BUFFER_INFO_srWindow = 10 -- 4 x Word16 Left,Top,Right,Bottom

c_STD_OUTPUT_HANDLE :: Word32
c_STD_OUTPUT_HANDLE = -11

foreign import stdcall unsafe "windows.h GetConsoleScreenBufferInfo"
    c_GetConsoleScreenBufferInfo :: HANDLE -> Ptr CONSOLE_SCREEN_BUFFER_INFO -> IO Bool

foreign import stdcall unsafe "windows.h GetStdHandle"
    c_GetStdHandle :: Word32 -> IO HANDLE


size :: Integral n => IO (Maybe (Window n))
size = do
    hdl <- c_GetStdHandle c_STD_OUTPUT_HANDLE
    allocaBytes sizeCONSOLE_SCREEN_BUFFER_INFO $ \p -> do
        b <- c_GetConsoleScreenBufferInfo hdl p
        if not b then return Nothing else do
            [left,top,right,bottom] <- forM [0..3] $ \i -> do
                v <- peekByteOff p ((i*2) + posCONSOLE_SCREEN_BUFFER_INFO_srWindow)
                return $ fromIntegral (v :: Word16)
            return $ Just $ Window (1+bottom-top) (1+right-left)
