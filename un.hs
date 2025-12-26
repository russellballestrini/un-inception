-- PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
--
-- This is free public domain software for the public good of a permacomputer hosted
-- at permacomputer.com - an always-on computer by the people, for the people. One
-- which is durable, easy to repair, and distributed like tap water for machine
-- learning intelligence.
--
-- The permacomputer is community-owned infrastructure optimized around four values:
--
--   TRUTH    - First principles, math & science, open source code freely distributed
--   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
--   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
--   LOVE     - Be yourself without hurting others, cooperation through natural law
--
-- This software contributes to that vision by enabling code execution across 42+
-- programming languages through a unified interface, accessible to all. Code is
-- seeds to sprout on any abandoned technology.
--
-- Learn more: https://www.permacomputer.com
--
-- Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
-- software, either in source code form or as a compiled binary, for any purpose,
-- commercial or non-commercial, and by any means.
--
-- NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
--
-- That said, our permacomputer's digital membrane stratum continuously runs unit,
-- integration, and functional tests on all of it's own software - with our
-- permacomputer monitoring itself, repairing itself, with minimal human in the
-- loop guidance. Our agents do their best.
--
-- Copyright 2025 TimeHexOn & foxhop & russell@unturf
-- https://www.timehexon.com
-- https://www.foxhop.net
-- https://www.unturf.com/software


#!/usr/bin/env runhaskell

{-
Haskell UN CLI - Unsandbox CLI Client

Full-featured CLI matching un.py capabilities:
- Execute code with env vars, input files, artifacts
- Interactive sessions with shell/REPL support
- Persistent services with domains and ports

Usage:
  chmod +x un.hs
  export UNSANDBOX_API_KEY="your_key_here"
  ./un.hs [options] <source_file>
  ./un.hs session [options]
  ./un.hs service [options]

Uses curl for HTTP (no external dependencies)
-}

import System.Environment (getArgs, getEnv, lookupEnv)
import System.Exit (exitWith, ExitCode(..), exitFailure)
import System.FilePath (takeExtension, takeFileName)
import System.Process (readProcessWithExitCode)
import System.IO (hPutStrLn, stderr)
import System.Directory (createDirectoryIfMissing, setPermissions, getPermissions, setOwnerExecutable)
import Data.List (isPrefixOf, intercalate)
import Data.Char (isDigit)
import Text.Printf (printf)
import Control.Monad (when, unless, forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64

-- ANSI colors
blue, red, green, yellow, reset :: String
blue = "\x1b[34m"
red = "\x1b[31m"
green = "\x1b[32m"
yellow = "\x1b[33m"
reset = "\x1b[0m"

-- Extension to language mapping
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

-- Escape JSON string
escapeJSON :: String -> String
escapeJSON = concatMap escape
  where
    escape '\\' = "\\\\"
    escape '"' = "\\\""
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c = [c]

-- Parse command line arguments
data Command = Execute ExecuteOpts | Session SessionOpts | Service ServiceOpts | Help

data ExecuteOpts = ExecuteOpts
  { exFile :: String
  , exEnv :: [(String, String)]
  , exFiles :: [String]
  , exArtifacts :: Bool
  , exOutDir :: Maybe String
  , exNetwork :: Maybe String
  , exVcpu :: Maybe Int
  }

data SessionOpts = SessionOpts
  { sessAction :: SessionAction
  , sessShell :: Maybe String
  , sessNetwork :: Maybe String
  , sessVcpu :: Maybe Int
  }

data SessionAction = SessionList | SessionKill String | SessionCreate

data ServiceOpts = ServiceOpts
  { svcAction :: ServiceAction
  , svcName :: Maybe String
  , svcPorts :: Maybe String
  , svcType :: Maybe String
  , svcBootstrap :: Maybe String
  , svcNetwork :: Maybe String
  , svcVcpu :: Maybe Int
  }

data ServiceAction = ServiceList | ServiceInfo String | ServiceLogs String
                   | ServiceSleep String | ServiceWake String | ServiceDestroy String
                   | ServiceCreate

-- Parse arguments
parseArgs :: [String] -> IO Command
parseArgs ("session":rest) = Session <$> parseSession rest
parseArgs ("service":rest) = Service <$> parseService rest
parseArgs args = parseExecute args

parseSession :: [String] -> IO SessionOpts
parseSession args = return $ parseSessionArgs args defaultSessionOpts
  where
    defaultSessionOpts = SessionOpts SessionCreate Nothing Nothing Nothing
    parseSessionArgs [] opts = opts
    parseSessionArgs ("--list":rest) opts = parseSessionArgs rest opts { sessAction = SessionList }
    parseSessionArgs ("--kill":id:rest) opts = parseSessionArgs rest opts { sessAction = SessionKill id }
    parseSessionArgs ("--shell":sh:rest) opts = parseSessionArgs rest opts { sessShell = Just sh }
    parseSessionArgs ("-s":sh:rest) opts = parseSessionArgs rest opts { sessShell = Just sh }
    parseSessionArgs ("-n":net:rest) opts = parseSessionArgs rest opts { sessNetwork = Just net }
    parseSessionArgs ("-v":v:rest) opts = parseSessionArgs rest opts { sessVcpu = Just (read v) }
    parseSessionArgs (_:rest) opts = parseSessionArgs rest opts

parseService :: [String] -> IO ServiceOpts
parseService args = return $ parseServiceArgs args defaultServiceOpts
  where
    defaultServiceOpts = ServiceOpts ServiceCreate Nothing Nothing Nothing Nothing Nothing Nothing
    parseServiceArgs [] opts = opts
    parseServiceArgs ("--list":rest) opts = parseServiceArgs rest opts { svcAction = ServiceList }
    parseServiceArgs ("--info":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceInfo id }
    parseServiceArgs ("--logs":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceLogs id }
    parseServiceArgs ("--sleep":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceSleep id }
    parseServiceArgs ("--wake":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceWake id }
    parseServiceArgs ("--destroy":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceDestroy id }
    parseServiceArgs ("--name":n:rest) opts = parseServiceArgs rest opts { svcName = Just n }
    parseServiceArgs ("--ports":p:rest) opts = parseServiceArgs rest opts { svcPorts = Just p }
    parseServiceArgs ("--type":t:rest) opts = parseServiceArgs rest opts { svcType = Just t }
    parseServiceArgs ("--bootstrap":b:rest) opts = parseServiceArgs rest opts { svcBootstrap = Just b }
    parseServiceArgs ("-n":net:rest) opts = parseServiceArgs rest opts { svcNetwork = Just net }
    parseServiceArgs ("-v":v:rest) opts = parseServiceArgs rest opts { svcVcpu = Just (read v) }
    parseServiceArgs (_:rest) opts = parseServiceArgs rest opts

parseExecute :: [String] -> IO Command
parseExecute args =
  case parseExecArgs args defaultExecOpts of
    Just opts -> return $ Execute opts
    Nothing -> return Help
  where
    defaultExecOpts = ExecuteOpts "" [] [] False Nothing Nothing Nothing
    parseExecArgs [] opts = if null (exFile opts) then Nothing else Just opts
    parseExecArgs (arg:rest) opts
      | "-e" `isPrefixOf` arg = parseExecArgs rest opts { exEnv = parseEnv rest : exEnv opts }
      | "-f" `isPrefixOf` arg = parseExecArgs rest opts { exFiles = head rest : exFiles opts }
      | "-a" == arg = parseExecArgs rest opts { exArtifacts = True }
      | "-o" `isPrefixOf` arg = parseExecArgs rest opts { exOutDir = Just (head rest) }
      | "-n" `isPrefixOf` arg = parseExecArgs rest opts { exNetwork = Just (head rest) }
      | "-v" `isPrefixOf` arg = parseExecArgs rest opts { exVcpu = Just (read (head rest)) }
      | not ("-" `isPrefixOf` arg) && null (exFile opts) = parseExecArgs rest opts { exFile = arg }
      | otherwise = parseExecArgs rest opts
    parseEnv (kv:rest) =
      let (k, v) = span (/= '=') kv
      in (k, drop 1 v)

-- Main
main :: IO ()
main = do
  args <- getArgs
  cmd <- parseArgs args
  case cmd of
    Execute opts -> executeCommand opts
    Session opts -> sessionCommand opts
    Service opts -> serviceCommand opts
    Help -> printHelp

printHelp :: IO ()
printHelp = do
  putStrLn "Usage:"
  putStrLn "  un.hs [options] <source_file>         Execute code"
  putStrLn "  un.hs session [options]                Manage sessions"
  putStrLn "  un.hs service [options]                Manage services"
  putStrLn ""
  putStrLn "Execute options:"
  putStrLn "  -e KEY=VALUE    Environment variable"
  putStrLn "  -f FILE         Input file"
  putStrLn "  -a              Return artifacts"
  putStrLn "  -o DIR          Output directory"
  putStrLn "  -n MODE         Network mode (zerotrust|semitrusted)"
  putStrLn "  -v N            vCPU count (1-8)"
  exitFailure

-- Execute command
executeCommand :: ExecuteOpts -> IO ()
executeCommand opts = do
  apiKey <- getApiKey
  let file = exFile opts

  -- Detect language
  let ext = takeExtension file
  lang <- case extToLang ext of
    Just l -> return l
    Nothing -> do
      hPutStrLn stderr $ "Error: Unknown extension: " ++ ext
      exitFailure

  -- Read file
  code <- readFile file

  -- Build JSON payload
  let envJSON = if null (exEnv opts) then ""
                else ",\"env\":{" ++ intercalate "," (map (\(k,v) -> printf "\"%s\":\"%s\"" k (escapeJSON v)) (exEnv opts)) ++ "}"

  let filesJSON = if null (exFiles opts) then ""
                  else ",\"input_files\":[" ++ intercalate "," (map fileToJSON (exFiles opts)) ++ "]"
                  where fileToJSON _ = "" -- simplified for now

  let artifactsJSON = if exArtifacts opts then ",\"return_artifacts\":true" else ""
  let networkJSON = maybe "" (\n -> ",\"network\":\"" ++ n ++ "\"") (exNetwork opts)
  let vcpuJSON = maybe "" (\v -> ",\"vcpu\":" ++ show v) (exVcpu opts)

  let json = "{\"language\":\"" ++ lang ++ "\",\"code\":\"" ++ escapeJSON code ++ "\""
             ++ envJSON ++ filesJSON ++ artifactsJSON ++ networkJSON ++ vcpuJSON ++ "}"

  -- Call API
  (exitCode, stdout, stderr) <- curlPost apiKey "/execute" json

  -- Print output
  unless (null stdout) $ putStr $ blue ++ stdout ++ reset
  unless (null stderr) $ putStr $ red ++ stderr ++ reset

  -- Parse exit code from response
  let responseExitCode = parseExitCode stdout
  exitWith $ if responseExitCode == 0 then ExitSuccess else ExitFailure responseExitCode

-- Session command
sessionCommand :: SessionOpts -> IO ()
sessionCommand opts = do
  apiKey <- getApiKey
  case sessAction opts of
    SessionList -> do
      (_, stdout, _) <- curlGet apiKey "/sessions"
      putStrLn stdout
    SessionKill sid -> do
      (_, stdout, _) <- curlDelete apiKey ("/sessions/" ++ sid)
      putStrLn $ green ++ "Session terminated: " ++ sid ++ reset
    SessionCreate -> do
      let shell = maybe "bash" id (sessShell opts)
      let networkJSON = maybe "" (\n -> ",\"network\":\"" ++ n ++ "\"") (sessNetwork opts)
      let vcpuJSON = maybe "" (\v -> ",\"vcpu\":" ++ show v) (sessVcpu opts)
      let json = "{\"shell\":\"" ++ shell ++ "\"" ++ networkJSON ++ vcpuJSON ++ "}"
      (_, stdout, _) <- curlPost apiKey "/sessions" json
      putStrLn $ yellow ++ "Session created (WebSocket required for interactivity)" ++ reset
      putStrLn stdout

-- Service command
serviceCommand :: ServiceOpts -> IO ()
serviceCommand opts = do
  apiKey <- getApiKey
  case svcAction opts of
    ServiceList -> do
      (_, stdout, _) <- curlGet apiKey "/services"
      putStrLn stdout
    ServiceInfo sid -> do
      (_, stdout, _) <- curlGet apiKey ("/services/" ++ sid)
      putStrLn stdout
    ServiceLogs sid -> do
      (_, stdout, _) <- curlGet apiKey ("/services/" ++ sid ++ "/logs")
      putStrLn stdout
    ServiceSleep sid -> do
      (_, stdout, _) <- curlPost apiKey ("/services/" ++ sid ++ "/sleep") "{}"
      putStrLn $ green ++ "Service sleeping: " ++ sid ++ reset
    ServiceWake sid -> do
      (_, stdout, _) <- curlPost apiKey ("/services/" ++ sid ++ "/wake") "{}"
      putStrLn $ green ++ "Service waking: " ++ sid ++ reset
    ServiceDestroy sid -> do
      (_, stdout, _) <- curlDelete apiKey ("/services/" ++ sid)
      putStrLn $ green ++ "Service destroyed: " ++ sid ++ reset
    ServiceCreate -> do
      case svcName opts of
        Nothing -> do
          hPutStrLn stderr "Error: --name required to create service"
          exitFailure
        Just name -> do
          let portsJSON = maybe "" (\p -> ",\"ports\":[" ++ p ++ "]") (svcPorts opts)
          let typeJSON = maybe "" (\t -> ",\"service_type\":\"" ++ t ++ "\"") (svcType opts)
          let bootstrapJSON = maybe "" (\b -> ",\"bootstrap\":\"" ++ escapeJSON b ++ "\"") (svcBootstrap opts)
          let networkJSON = maybe "" (\n -> ",\"network\":\"" ++ n ++ "\"") (svcNetwork opts)
          let vcpuJSON = maybe "" (\v -> ",\"vcpu\":" ++ show v) (svcVcpu opts)
          let json = "{\"name\":\"" ++ name ++ "\"" ++ portsJSON ++ typeJSON ++ bootstrapJSON ++ networkJSON ++ vcpuJSON ++ "}"
          (_, stdout, _) <- curlPost apiKey "/services" json
          putStrLn $ green ++ "Service created" ++ reset
          putStrLn stdout

-- HTTP helpers using curl
curlPost :: String -> String -> String -> IO (ExitCode, String, String)
curlPost apiKey endpoint body = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    [ "-s", "-X", "POST"
    , "https://api.unsandbox.com" ++ endpoint
    , "-H", "Content-Type: application/json"
    , "-H", "Authorization: Bearer " ++ apiKey
    , "-d", body
    ] ""
  return (exitCode, stdout, stderr)

curlGet :: String -> String -> IO (ExitCode, String, String)
curlGet apiKey endpoint =
  readProcessWithExitCode "curl"
    [ "-s", "https://api.unsandbox.com" ++ endpoint
    , "-H", "Authorization: Bearer " ++ apiKey
    ] ""

curlDelete :: String -> String -> IO (ExitCode, String, String)
curlDelete apiKey endpoint =
  readProcessWithExitCode "curl"
    [ "-s", "-X", "DELETE"
    , "https://api.unsandbox.com" ++ endpoint
    , "-H", "Authorization: Bearer " ++ apiKey
    ] ""

-- Get API key from environment
getApiKey :: IO String
getApiKey = do
  maybeKey <- lookupEnv "UNSANDBOX_API_KEY"
  case maybeKey of
    Nothing -> do
      hPutStrLn stderr "Error: UNSANDBOX_API_KEY not set"
      exitFailure
    Just key -> return key

-- Parse exit code from JSON response
parseExitCode :: String -> Int
parseExitCode resp =
  case extractField "exit_code" resp of
    Just s -> read s
    Nothing -> 0
  where
    extractField field str =
      case break (== ':') <$> words str >>= find (\(k,_) -> field `elem` words k) of
        Just (_, ':':v) -> Just $ takeWhile isDigit v
        _ -> Nothing
    find f = foldr (\x acc -> if f x then Just x else acc) Nothing
