import System.Console.Terminal.Size
import System.IO
import GHC.IO.Handle
import Control.Monad
import Data.Maybe

stdHandles =
  [ (stdin, "stdin")
  , (stdout, "stdout")
  , (stderr, "stderr")
  ]

main = do
  fh <- openFile "test.hs" ReadMode
  forM_ stdHandles $ \(h, n) -> do
    putStrLn $ "With redirected " ++ n
    -- save handle
    h_saved <- hDuplicate h
    -- redirect to a file handle
    hDuplicateTo fh h
    -- run hSize on all three std handles
    hSizes <- forM stdHandles (hSize . fst)
    -- restore redirected handle
    hDuplicateTo h_saved h
    -- report sizes
    forM_ (zip hSizes stdHandles) $ \(s, (h', n')) -> do
      putStrLn $ "  hSize " ++ n' ++ " = " ++ show s
