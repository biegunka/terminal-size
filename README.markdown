terminal-size
=============

[![Hackage](https://img.shields.io/hackage/v/terminal-size.svg?style=flat)](https://hackage.haskell.org/package/terminal-size)
[![Build Status](https://travis-ci.org/biegunka/terminal-size.png)](https://travis-ci.org/biegunka/terminal-size)

Get terminal window width and height

Usage
-----

```
>>> import System.Console.Terminal.Size
>>> size
Just (Window {height = 60, width = 112})
```

Test
----

Compile test.hs and run it in a terminal. Here is what I get on Linux:

```
> ghc test.hs
> ./test
With redirected stdin
  hSize stdin = Nothing
  hSize stdout = Just (Window {height = 19, width = 87})
  hSize stderr = Just (Window {height = 19, width = 87})
With redirected stdout
  hSize stdin = Just (Window {height = 19, width = 87})
  hSize stdout = Nothing
  hSize stderr = Just (Window {height = 19, width = 87})
With redirected stderr
  hSize stdin = Just (Window {height = 19, width = 87})
  hSize stdout = Just (Window {height = 19, width = 87})
  hSize stderr = Nothing
```

On MINGW/MSYS the output is the same.

On Windows with cmd.exe I get

```
With redirected stdin
  hSize stdin = Nothing
  hSize stdout = Just (Window {height = 40, width = 164})
  hSize stderr = Just (Window {height = 40, width = 164})
With redirected stdout
  hSize stdin = Nothing
  hSize stdout = Nothing
  hSize stderr = Just (Window {height = 40, width = 164})
With redirected stderr
  hSize stdin = Nothing
  hSize stdout = Just (Window {height = 40, width = 164})
  hSize stderr = Nothing
```
