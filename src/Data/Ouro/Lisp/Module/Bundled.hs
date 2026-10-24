{-# LANGUAGE TemplateHaskell #-}

-- The modules that ship inside the compiler. Their sources are the files in lib/, embedded
-- when the compiler is built, so (import std) needs no file on disk and no install path,
-- and resolving a bundled module is pure.
module Data.Ouro.Lisp.Module.Bundled
( bundled
) where

import           Data.ByteString    (ByteString)
import           Data.FileEmbed     (embedDir)
import qualified Data.Map.Strict    as Map
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE


-- Each bundled module's file name, for positions, and its source, by its name: lib/std.ouro is std
bundled :: Map.Map Text (FilePath, Text)
bundled = Map.fromList
    [ (name, ("lib/" <> path, TE.decodeUtf8 bytes))
    | (path, bytes) <- files
    , Just name     <- [T.stripSuffix ".ouro" (T.pack path)]
    ]
    where
    files :: [(FilePath, ByteString)]
    files = $(embedDir "lib")
