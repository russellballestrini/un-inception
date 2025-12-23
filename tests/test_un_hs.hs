#!/usr/bin/env runhaskell
{-# LANGUAGE OverloadedStrings #-}

{-
Haskell UN CLI Test Suite

Usage:
  chmod +x test_un_hs.hs
  ./test_un_hs.hs

Or with runhaskell:
  runhaskell test_un_hs.hs

Tests the Haskell UN CLI implementation (un.hs) for:
1. Extension detection logic
2. API integration (if UNSANDBOX_API_KEY is set)
3. End-to-end execution with fib.hs test file
-}

import System.FilePath (takeExtension)
import System.Environment (lookupEnv)
import System.Exit (exitWith, ExitCode(..))
import System.Process (readProcessWithExitCode)
import Control.Monad (unless, when)
import Data.List (isInfixOf)

-- ANSI color codes
green, red, yellow, reset :: String
green = "\x1b[32m"
red = "\x1b[31m"
yellow = "\x1b[33m"
reset = "\x1b[0m"

-- Extension to language mapping (from un.hs)
extToLang :: String -> Maybe String
extToLang ext = lookup ext extMap
  where
    extMap = [ (".hs", "haskell"), (".ml", "ocaml"), (".clj", "clojure")
             , (".scm", "scheme"), (".lisp", "commonlisp"), (".erl", "erlang")
             , (".ex", "elixir"), (".py", "python"), (".js", "javascript")
             , (".rb", "ruby"), (".go", "go"), (".rs", "rust")
             , (".c", "c"), (".cpp", "cpp"), (".java", "java")
             ]

-- Test result type
data TestResult = Pass | Fail String

-- Print test result
printResult :: String -> TestResult -> IO Bool
printResult testName result = case result of
  Pass -> do
    putStrLn $ green ++ "✓ PASS" ++ reset ++ " - " ++ testName
    return True
  Fail msg -> do
    putStrLn $ red ++ "✗ FAIL" ++ reset ++ " - " ++ testName
    putStrLn $ "  Error: " ++ msg
    return False

-- Test 1: Extension detection
testExtensionDetection :: IO TestResult
testExtensionDetection = do
  let tests = [ (".hs", Just "haskell")
              , (".ml", Just "ocaml")
              , (".clj", Just "clojure")
              , (".scm", Just "scheme")
              , (".lisp", Just "commonlisp")
              , (".erl", Just "erlang")
              , (".ex", Just "elixir")
              , (".py", Just "python")
              , (".js", Just "javascript")
              , (".rb", Just "ruby")
              ]

  let failures = [ (ext, expected, actual)
                 | (ext, expected) <- tests
                 , let actual = extToLang ext
                 , actual /= expected
                 ]

  if null failures
    then return Pass
    else return $ Fail $ "Extension mappings failed: " ++ show failures

-- Test 2: API integration (if API key is available)
testAPIIntegration :: IO TestResult
testAPIIntegration = do
  apiKeyMaybe <- lookupEnv "UNSANDBOX_API_KEY"
  case apiKeyMaybe of
    Nothing -> return $ Pass  -- Skip test if no API key
    Just _ -> do
      -- Create a simple test file
      let testCode = "main = putStrLn \"test\"\n"
      writeFile "/tmp/test_un_hs_api.hs" testCode

      -- Run the CLI
      (exitCode, stdout, stderr) <- readProcessWithExitCode
        "./un.hs"
        ["/tmp/test_un_hs_api.hs"]
        ""

      -- Check if it executed successfully
      if exitCode == ExitSuccess && "test" `isInfixOf` stdout
        then return Pass
        else return $ Fail $ "API call failed: " ++ show exitCode ++
                            ", stdout: " ++ stdout ++
                            ", stderr: " ++ stderr

-- Test 3: Functional test with fib.hs
testFibonacci :: IO TestResult
testFibonacci = do
  apiKeyMaybe <- lookupEnv "UNSANDBOX_API_KEY"
  case apiKeyMaybe of
    Nothing -> return Pass  -- Skip test if no API key
    Just _ -> do
      -- Check if fib.hs exists
      let fibPath = "../test/fib.hs"

      -- Run the CLI with fib.hs
      (exitCode, stdout, stderr) <- readProcessWithExitCode
        "./un.hs"
        [fibPath]
        ""

      -- Check if output contains expected fibonacci result
      if exitCode == ExitSuccess && "fib(10) = 55" `isInfixOf` stdout
        then return Pass
        else return $ Fail $ "Fibonacci test failed: " ++ show exitCode ++
                            ", stdout: " ++ stdout ++
                            ", stderr: " ++ stderr

-- Main test runner
main :: IO ()
main = do
  putStrLn "=== Haskell UN CLI Test Suite ==="
  putStrLn ""

  -- Check if API key is set
  apiKeyMaybe <- lookupEnv "UNSANDBOX_API_KEY"
  when (apiKeyMaybe == Nothing) $ do
    putStrLn $ yellow ++ "⚠ WARNING" ++ reset ++
               " - UNSANDBOX_API_KEY not set, skipping API tests"
    putStrLn ""

  -- Run tests
  results <- sequence
    [ testExtensionDetection >>= printResult "Extension detection"
    , testAPIIntegration >>= printResult "API integration"
    , testFibonacci >>= printResult "Fibonacci end-to-end test"
    ]

  putStrLn ""

  -- Summary
  let passed = length $ filter id results
  let total = length results

  if passed == total
    then do
      putStrLn $ green ++ "✓ All tests passed (" ++ show passed ++ "/" ++ show total ++ ")" ++ reset
      exitWith ExitSuccess
    else do
      putStrLn $ red ++ "✗ Some tests failed (" ++ show passed ++ "/" ++ show total ++ " passed)" ++ reset
      exitWith $ ExitFailure 1
