#!/usr/bin/env runhaskell

{-
Unit Tests for Un Haskell SDK Library Functions

Run with: runhaskell test_library.hs
No credentials required - tests pure library functions only.
-}

import System.Exit (exitFailure, exitSuccess)
import Data.Char (isHexDigit)
import Data.List (isPrefixOf)

-- Import from parent src directory
-- In a real scenario, this would be properly imported

-- ANSI colors
blue, red, green, yellow, reset :: String
blue = "\x1b[34m"
red = "\x1b[31m"
green = "\x1b[32m"
yellow = "\x1b[33m"
reset = "\x1b[0m"

-- Inline implementation for testing (matches un.hs)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Crypto.Hash.SHA256 (hmac)
import Text.Printf (printf)
import System.FilePath (takeExtension)

hmacSha256 :: String -> String -> String
hmacSha256 secret message =
  let secretBS = BSC.pack secret
      messageBS = BSC.pack message
      mac = hmac secretBS messageBS
  in concatMap (printf "%02x") (BS.unpack mac)

extToLang :: String -> Maybe String
extToLang ext = lookup ext extMap
  where
    extMap = [ (".hs", "haskell"), (".ml", "ocaml"), (".clj", "clojure")
             , (".scm", "scheme"), (".lisp", "commonlisp"), (".erl", "erlang")
             , (".ex", "elixir"), (".exs", "elixir"), (".py", "python")
             , (".js", "javascript"), (".ts", "typescript"), (".rb", "ruby")
             , (".go", "go"), (".rs", "rust"), (".c", "c"), (".cpp", "cpp")
             , (".cc", "cpp"), (".cxx", "cpp"), (".java", "java")
             , (".kt", "kotlin"), (".cs", "csharp"), (".fs", "fsharp")
             , (".jl", "julia"), (".r", "r"), (".cr", "crystal")
             , (".d", "d"), (".nim", "nim"), (".zig", "zig"), (".v", "v")
             , (".dart", "dart"), (".groovy", "groovy"), (".scala", "scala")
             , (".sh", "bash"), (".pl", "perl"), (".lua", "lua"), (".php", "php")
             ]

sdkVersion :: String
sdkVersion = "4.2.0"

detectLanguage :: String -> Maybe String
detectLanguage filename = extToLang (takeExtension filename)

hmacSign :: String -> String -> String
hmacSign = hmacSha256

main :: IO ()
main = do
  putStrLn $ "\n" ++ blue ++ "=== Un Haskell SDK Library Tests ===" ++ reset ++ "\n"

  results <- sequence
    [ runTest "version" testVersion
    , runTest "detect_language" testDetectLanguage
    , runTest "hmac_sign" testHmacSign
    , runTest "hmac_sign_deterministic" testHmacSignDeterministic
    , runTest "hmac_sign_different_secrets" testHmacSignDifferentSecrets
    ]

  let passed = length $ filter id results
  let failed = length $ filter not results
  let total = length results

  putStrLn $ "\n" ++ blue ++ "Results: " ++ show passed ++ "/" ++ show total ++ " passed" ++ reset

  if failed > 0
    then do
      putStrLn $ red ++ show failed ++ " test(s) failed" ++ reset
      exitFailure
    else do
      putStrLn $ green ++ "All tests passed!" ++ reset
      exitSuccess

runTest :: String -> IO Bool -> IO Bool
runTest name test = do
  result <- test
  if result
    then putStrLn $ green ++ "PASS" ++ reset ++ ": " ++ name
    else putStrLn $ red ++ "FAIL" ++ reset ++ ": " ++ name
  return result

-- ============================================================================
-- Unit Tests
-- ============================================================================

testVersion :: IO Bool
testVersion = do
  let version = sdkVersion
  -- Check it's non-empty
  if null version
    then return False
    else do
      -- Check it's semver format (contains dots)
      let parts = words $ map (\c -> if c == '.' then ' ' else c) version
      return $ length parts == 3

testDetectLanguage :: IO Bool
testDetectLanguage = do
  -- Test common extensions
  let tests =
        [ (detectLanguage "script.py" == Just "python", "python")
        , (detectLanguage "app.js" == Just "javascript", "javascript")
        , (detectLanguage "main.go" == Just "go", "go")
        , (detectLanguage "main.rs" == Just "rust", "rust")
        , (detectLanguage "main.c" == Just "c", "c")
        , (detectLanguage "main.cpp" == Just "cpp", "cpp")
        , (detectLanguage "Main.java" == Just "java", "java")
        , (detectLanguage "script.rb" == Just "ruby", "ruby")
        , (detectLanguage "script.sh" == Just "bash", "bash")
        , (detectLanguage "script.lua" == Just "lua", "lua")
        , (detectLanguage "script.pl" == Just "perl", "perl")
        , (detectLanguage "index.php" == Just "php", "php")
        , (detectLanguage "main.hs" == Just "haskell", "haskell")
        , (detectLanguage "main.ml" == Just "ocaml", "ocaml")
        , (detectLanguage "main.ex" == Just "elixir", "elixir")
        , (detectLanguage "main.erl" == Just "erlang", "erlang")
        -- Test with paths
        , (detectLanguage "/path/to/script.py" == Just "python", "path/python")
        -- Test unknown extensions
        , (detectLanguage "Makefile" == Nothing, "Makefile")
        , (detectLanguage "README" == Nothing, "README")
        , (detectLanguage "script.unknown" == Nothing, "unknown")
        ]
  return $ all fst tests

testHmacSign :: IO Bool
testHmacSign = do
  let signature = hmacSign "my_secret" "test message"
  -- Should be 64 hex characters
  let is64Hex = length signature == 64 && all isHexDigit signature
  -- Should be lowercase
  let isLowercase = all (\c -> not (c >= 'A' && c <= 'F')) signature
  return $ is64Hex && isLowercase

testHmacSignDeterministic :: IO Bool
testHmacSignDeterministic = do
  let sig1 = hmacSign "test_secret" "same message"
  let sig2 = hmacSign "test_secret" "same message"
  return $ sig1 == sig2

testHmacSignDifferentSecrets :: IO Bool
testHmacSignDifferentSecrets = do
  let sig1 = hmacSign "secret1" "test message"
  let sig2 = hmacSign "secret2" "test message"
  return $ sig1 /= sig2
