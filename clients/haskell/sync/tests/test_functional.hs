#!/usr/bin/env runhaskell

{-
Functional Tests for Un Haskell SDK

Run with: runhaskell test_functional.hs
Requires: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY environment variables

These tests make real API calls to api.unsandbox.com
-}

import System.Exit (exitFailure, exitSuccess)
import System.Environment (lookupEnv)
import System.Process (readProcessWithExitCode)
import Data.List (isPrefixOf, isInfixOf)
import Data.Char (isDigit)

-- ANSI colors
blue, red, green, yellow, reset :: String
blue = "\x1b[34m"
red = "\x1b[31m"
green = "\x1b[32m"
yellow = "\x1b[33m"
reset = "\x1b[0m"

-- API constants
apiBase :: String
apiBase = "https://api.unsandbox.com"

portalBase :: String
portalBase = "https://unsandbox.com"

-- Import HMAC from crypto library
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Crypto.Hash.SHA256 (hmac)
import Text.Printf (printf)
import Data.Time.Clock.POSIX (getPOSIXTime)

hmacSha256 :: String -> String -> String
hmacSha256 secret message =
  let secretBS = BSC.pack secret
      messageBS = BSC.pack message
      mac = hmac secretBS messageBS
  in concatMap (printf "%02x") (BS.unpack mac)

main :: IO ()
main = do
  putStrLn $ "\n" ++ blue ++ "=== Un Haskell SDK Functional Tests ===" ++ reset ++ "\n"

  -- Check for credentials
  publicKey <- lookupEnv "UNSANDBOX_PUBLIC_KEY"
  secretKey <- lookupEnv "UNSANDBOX_SECRET_KEY"

  case (publicKey, secretKey) of
    (Just pk, Just sk) -> do
      results <- sequence
        [ runTest "health_check" (testHealthCheck pk sk)
        , runTest "validate_keys" (testValidateKeys pk sk)
        , runTest "execute_python" (testExecutePython pk sk)
        , runTest "execute_with_error" (testExecuteWithError pk sk)
        , runTest "session_list" (testSessionList pk sk)
        , runTest "service_list" (testServiceList pk sk)
        , runTest "snapshot_list" (testSnapshotList pk sk)
        , runTest "image_list" (testImageList pk sk)
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
          putStrLn $ green ++ "All functional tests passed!" ++ reset
          exitSuccess

    _ -> do
      putStrLn $ yellow ++ "SKIP: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set" ++ reset
      exitSuccess

runTest :: String -> IO Bool -> IO Bool
runTest name test = do
  putStr $ "  Running " ++ name ++ "... "
  result <- test
  if result
    then putStrLn $ green ++ "PASS" ++ reset
    else putStrLn $ red ++ "FAIL" ++ reset
  return result

-- Build auth headers
buildAuthHeaders :: String -> String -> String -> String -> String -> IO [String]
buildAuthHeaders publicKey secretKey method path body = do
  now <- getPOSIXTime
  let timestamp = show (floor now :: Integer)
  let message = timestamp ++ ":" ++ method ++ ":" ++ path ++ ":" ++ body
  let signature = hmacSha256 secretKey message
  return [ "-H", "Authorization: Bearer " ++ publicKey
         , "-H", "X-Timestamp: " ++ timestamp
         , "-H", "X-Signature: " ++ signature
         ]

-- HTTP helpers
curlGet :: String -> String -> String -> IO String
curlGet publicKey secretKey endpoint = do
  authHeaders <- buildAuthHeaders publicKey secretKey "GET" endpoint ""
  (_, stdout, _) <- readProcessWithExitCode "curl"
    (["-s", apiBase ++ endpoint] ++ authHeaders) ""
  return stdout

curlPost :: String -> String -> String -> String -> IO String
curlPost publicKey secretKey endpoint json = do
  authHeaders <- buildAuthHeaders publicKey secretKey "POST" endpoint json
  (_, stdout, _) <- readProcessWithExitCode "curl"
    (["-s", "-X", "POST", apiBase ++ endpoint, "-H", "Content-Type: application/json"] ++ authHeaders ++ ["-d", json]) ""
  return stdout

curlPostPortal :: String -> String -> String -> String -> IO String
curlPostPortal publicKey secretKey endpoint json = do
  authHeaders <- buildAuthHeaders publicKey secretKey "POST" endpoint json
  (_, stdout, _) <- readProcessWithExitCode "curl"
    (["-s", "-X", "POST", portalBase ++ endpoint, "-H", "Content-Type: application/json"] ++ authHeaders ++ ["-d", json]) ""
  return stdout

-- ============================================================================
-- Functional Tests
-- ============================================================================

testHealthCheck :: String -> String -> IO Bool
testHealthCheck _ _ = do
  (_, stdout, _) <- readProcessWithExitCode "curl"
    ["-s", "-o", "/dev/null", "-w", "%{http_code}", apiBase ++ "/health"] ""
  return $ filter isDigit stdout == "200"

testValidateKeys :: String -> String -> IO Bool
testValidateKeys publicKey secretKey = do
  response <- curlPostPortal publicKey secretKey "/keys/validate" "{}"
  -- Check response is JSON with expected fields
  return $ "{" `isPrefixOf` response && ("\"valid\"" `isInfixOf` response || "\"status\"" `isInfixOf` response)

testExecutePython :: String -> String -> IO Bool
testExecutePython publicKey secretKey = do
  let json = "{\"language\":\"python\",\"code\":\"print(6 * 7)\"}"
  response <- curlPost publicKey secretKey "/execute" json
  -- Check output contains 42
  return $ "42" `isInfixOf` response

testExecuteWithError :: String -> String -> IO Bool
testExecuteWithError publicKey secretKey = do
  let json = "{\"language\":\"python\",\"code\":\"import sys; sys.exit(1)\"}"
  response <- curlPost publicKey secretKey "/execute" json
  -- Check exit_code is 1
  return $ "\"exit_code\":1" `isInfixOf` response || "\"exit_code\": 1" `isInfixOf` response

testSessionList :: String -> String -> IO Bool
testSessionList publicKey secretKey = do
  response <- curlGet publicKey secretKey "/sessions"
  -- Response should be JSON array or object
  let trimmed = dropWhile (== ' ') response
  return $ "[" `isPrefixOf` trimmed || "{" `isPrefixOf` trimmed

testServiceList :: String -> String -> IO Bool
testServiceList publicKey secretKey = do
  response <- curlGet publicKey secretKey "/services"
  let trimmed = dropWhile (== ' ') response
  return $ "[" `isPrefixOf` trimmed || "{" `isPrefixOf` trimmed

testSnapshotList :: String -> String -> IO Bool
testSnapshotList publicKey secretKey = do
  response <- curlGet publicKey secretKey "/snapshots"
  let trimmed = dropWhile (== ' ') response
  return $ "[" `isPrefixOf` trimmed || "{" `isPrefixOf` trimmed

testImageList :: String -> String -> IO Bool
testImageList publicKey secretKey = do
  response <- curlGet publicKey secretKey "/images"
  let trimmed = dropWhile (== ' ') response
  return $ "[" `isPrefixOf` trimmed || "{" `isPrefixOf` trimmed
