{-# LANGUAGE CPP #-}

module Main (main) where

import           CLI.Parser           (runParser)
import           CLI.Runner           (runCommand)
import           System.IO            (hSetEncoding, stderr, stdout, utf8)
#if defined(mingw32_HOST_OS)
import           Control.Exception    (IOException, bracket_, catch)
import           System.Win32.Console (getConsoleOutputCP, setConsoleOutputCP)
#endif

main :: IO ()
main = withUtf8Console $ do
    -- Messages and diagnostics are written as UTF-8, whatever the platform's code page
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    runParser >>= runCommand


-- A Windows console decodes what it is sent with its own code page, such as 850, so it is
-- switched to UTF-8 (65001) while Ouro runs and restored afterwards, even when Ouro exits
-- with an error. Without a console (output redirected to a file or a pipe) the code page is
-- 0 and nothing is changed. Elsewhere terminals are left as they are.
withUtf8Console :: IO a -> IO a
#if defined(mingw32_HOST_OS)
withUtf8Console action = do
    previous <- getConsoleOutputCP
    case previous of
        0           -> action
        _hasConsole -> bracket_ (trySet 65001) (trySet previous) action
    where
    trySet codePage = setConsoleOutputCP codePage `catch` \(_ :: IOException) -> pure ()
#else
withUtf8Console = id
#endif
