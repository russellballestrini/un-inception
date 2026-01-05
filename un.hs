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
import Data.Char (isDigit, ord)
import Text.Printf (printf)
import Control.Monad (when, unless, forM_)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64
import Crypto.Hash.SHA256 (hmac)
import Numeric (showHex)
import Data.Time.Clock.POSIX (getPOSIXTime)

-- API constants
apiBase :: String
apiBase = "https://api.unsandbox.com"

portalBase :: String
portalBase = "https://unsandbox.com"

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
data Command = Execute ExecuteOpts | Session SessionOpts | Service ServiceOpts | Key KeyOpts | Snapshot SnapshotOpts | Help

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
  , sessFiles :: [String]
  , sessSnapshotName :: Maybe String
  , sessSnapshotFrom :: Maybe String
  , sessHot :: Bool
  }

data SessionAction = SessionList | SessionKill String | SessionCreate
                   | SessionSnapshot String | SessionRestore String

data ServiceOpts = ServiceOpts
  { svcAction :: ServiceAction
  , svcName :: Maybe String
  , svcPorts :: Maybe String
  , svcType :: Maybe String
  , svcBootstrap :: Maybe String
  , svcBootstrapFile :: Maybe String
  , svcNetwork :: Maybe String
  , svcVcpu :: Maybe Int
  , svcFiles :: [String]
  , svcSnapshotName :: Maybe String
  , svcSnapshotFrom :: Maybe String
  , svcHot :: Bool
  }

data ServiceAction = ServiceList | ServiceInfo String | ServiceLogs String
                   | ServiceSleep String | ServiceWake String | ServiceDestroy String
                   | ServiceExecute String String | ServiceDumpBootstrap String (Maybe String)
                   | ServiceCreate | ServiceSnapshot String | ServiceRestore String

data SnapshotOpts = SnapshotOpts
  { snapAction :: SnapshotAction
  , snapCloneType :: Maybe String
  , snapCloneName :: Maybe String
  , snapClonePorts :: Maybe String
  }

data SnapshotAction = SnapshotList | SnapshotInfo String | SnapshotDelete String
                    | SnapshotClone String

data KeyOpts = KeyOpts
  { keyExtend :: Bool
  }

-- Parse arguments
parseArgs :: [String] -> IO Command
parseArgs ("session":rest) = Session <$> parseSession rest
parseArgs ("service":rest) = Service <$> parseService rest
parseArgs ("key":rest) = Key <$> parseKey rest
parseArgs ("snapshot":rest) = Snapshot <$> parseSnapshot rest
parseArgs args = parseExecute args

parseKey :: [String] -> IO KeyOpts
parseKey args = return $ parseKeyArgs args defaultKeyOpts
  where
    defaultKeyOpts = KeyOpts False
    parseKeyArgs [] opts = opts
    parseKeyArgs ("--extend":rest) opts = parseKeyArgs rest opts { keyExtend = True }
    parseKeyArgs (_:rest) opts = parseKeyArgs rest opts

parseSnapshot :: [String] -> IO SnapshotOpts
parseSnapshot args = return $ parseSnapshotArgs args defaultSnapshotOpts
  where
    defaultSnapshotOpts = SnapshotOpts SnapshotList Nothing Nothing Nothing
    parseSnapshotArgs [] opts = opts
    parseSnapshotArgs ("--list":rest) opts = parseSnapshotArgs rest opts { snapAction = SnapshotList }
    parseSnapshotArgs ("-l":rest) opts = parseSnapshotArgs rest opts { snapAction = SnapshotList }
    parseSnapshotArgs ("--info":id:rest) opts = parseSnapshotArgs rest opts { snapAction = SnapshotInfo id }
    parseSnapshotArgs ("--delete":id:rest) opts = parseSnapshotArgs rest opts { snapAction = SnapshotDelete id }
    parseSnapshotArgs ("--clone":id:rest) opts = parseSnapshotArgs rest opts { snapAction = SnapshotClone id }
    parseSnapshotArgs ("--type":t:rest) opts = parseSnapshotArgs rest opts { snapCloneType = Just t }
    parseSnapshotArgs ("--name":n:rest) opts = parseSnapshotArgs rest opts { snapCloneName = Just n }
    parseSnapshotArgs ("--ports":p:rest) opts = parseSnapshotArgs rest opts { snapClonePorts = Just p }
    parseSnapshotArgs (_:rest) opts = parseSnapshotArgs rest opts

parseSession :: [String] -> IO SessionOpts
parseSession args = return $ parseSessionArgs args defaultSessionOpts
  where
    defaultSessionOpts = SessionOpts SessionCreate Nothing Nothing Nothing [] Nothing Nothing False
    parseSessionArgs [] opts = opts
    parseSessionArgs ("--list":rest) opts = parseSessionArgs rest opts { sessAction = SessionList }
    parseSessionArgs ("--kill":id:rest) opts = parseSessionArgs rest opts { sessAction = SessionKill id }
    parseSessionArgs ("--snapshot":id:rest) opts = parseSessionArgs rest opts { sessAction = SessionSnapshot id }
    parseSessionArgs ("--restore":id:rest) opts = parseSessionArgs rest opts { sessAction = SessionRestore id }
    parseSessionArgs ("--shell":sh:rest) opts = parseSessionArgs rest opts { sessShell = Just sh }
    parseSessionArgs ("-s":sh:rest) opts = parseSessionArgs rest opts { sessShell = Just sh }
    parseSessionArgs ("--snapshot-name":n:rest) opts = parseSessionArgs rest opts { sessSnapshotName = Just n }
    parseSessionArgs ("--from":f:rest) opts = parseSessionArgs rest opts { sessSnapshotFrom = Just f }
    parseSessionArgs ("--hot":rest) opts = parseSessionArgs rest opts { sessHot = True }
    parseSessionArgs ("-n":net:rest) opts = parseSessionArgs rest opts { sessNetwork = Just net }
    parseSessionArgs ("-v":v:rest) opts = parseSessionArgs rest opts { sessVcpu = Just (read v) }
    parseSessionArgs ("-f":f:rest) opts = parseSessionArgs rest opts { sessFiles = sessFiles opts ++ [f] }
    parseSessionArgs (_:rest) opts = parseSessionArgs rest opts

parseService :: [String] -> IO ServiceOpts
parseService args = return $ parseServiceArgs args defaultServiceOpts
  where
    defaultServiceOpts = ServiceOpts ServiceCreate Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing False
    parseServiceArgs [] opts = opts
    parseServiceArgs ("--list":rest) opts = parseServiceArgs rest opts { svcAction = ServiceList }
    parseServiceArgs ("--info":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceInfo id }
    parseServiceArgs ("--logs":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceLogs id }
    parseServiceArgs ("--freeze":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceSleep id }
    parseServiceArgs ("--unfreeze":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceWake id }
    parseServiceArgs ("--destroy":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceDestroy id }
    parseServiceArgs ("--snapshot":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceSnapshot id }
    parseServiceArgs ("--restore":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceRestore id }
    parseServiceArgs ("--execute":id:"--command":cmd:rest) opts = parseServiceArgs rest opts { svcAction = ServiceExecute id cmd }
    parseServiceArgs ("--dump-bootstrap":id:file:rest) opts = parseServiceArgs rest opts { svcAction = ServiceDumpBootstrap id (Just file) }
    parseServiceArgs ("--dump-bootstrap":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceDumpBootstrap id Nothing }
    parseServiceArgs ("--name":n:rest) opts = parseServiceArgs rest opts { svcName = Just n }
    parseServiceArgs ("--ports":p:rest) opts = parseServiceArgs rest opts { svcPorts = Just p }
    parseServiceArgs ("--type":t:rest) opts = parseServiceArgs rest opts { svcType = Just t }
    parseServiceArgs ("--bootstrap":b:rest) opts = parseServiceArgs rest opts { svcBootstrap = Just b }
    parseServiceArgs ("--bootstrap-file":f:rest) opts = parseServiceArgs rest opts { svcBootstrapFile = Just f }
    parseServiceArgs ("--snapshot-name":n:rest) opts = parseServiceArgs rest opts { svcSnapshotName = Just n }
    parseServiceArgs ("--from":f:rest) opts = parseServiceArgs rest opts { svcSnapshotFrom = Just f }
    parseServiceArgs ("--hot":rest) opts = parseServiceArgs rest opts { svcHot = True }
    parseServiceArgs ("-n":net:rest) opts = parseServiceArgs rest opts { svcNetwork = Just net }
    parseServiceArgs ("-v":v:rest) opts = parseServiceArgs rest opts { svcVcpu = Just (read v) }
    parseServiceArgs ("-f":f:rest) opts = parseServiceArgs rest opts { svcFiles = svcFiles opts ++ [f] }
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
      | "-" `isPrefixOf` arg = do
        hPutStrLn stderr $ red ++ "Unknown option: " ++ arg ++ reset
        exitFailure
      | null (exFile opts) = parseExecArgs rest opts { exFile = arg }
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
    Key opts -> keyCommand opts
    Snapshot opts -> snapshotCommand opts
    Help -> printHelp

printHelp :: IO ()
printHelp = do
  putStrLn "Usage:"
  putStrLn "  un.hs [options] <source_file>         Execute code"
  putStrLn "  un.hs session [options]                Manage sessions"
  putStrLn "  un.hs service [options]                Manage services"
  putStrLn "  un.hs snapshot [options]               Manage snapshots"
  putStrLn "  un.hs key [options]                    Validate/extend API key"
  putStrLn ""
  putStrLn "Execute options:"
  putStrLn "  -e KEY=VALUE    Environment variable"
  putStrLn "  -f FILE         Input file"
  putStrLn "  -a              Return artifacts"
  putStrLn "  -o DIR          Output directory"
  putStrLn "  -n MODE         Network mode (zerotrust|semitrusted)"
  putStrLn "  -v N            vCPU count (1-8)"
  putStrLn ""
  putStrLn "Session snapshot options:"
  putStrLn "  --snapshot ID        Create snapshot of session"
  putStrLn "  --restore ID         Restore session from snapshot"
  putStrLn "  --from SNAPSHOT_ID   Snapshot ID to restore from"
  putStrLn "  --snapshot-name NAME Optional snapshot name"
  putStrLn "  --hot                Take live snapshot without freezing"
  putStrLn ""
  putStrLn "Service snapshot options:"
  putStrLn "  --snapshot ID        Create snapshot of service"
  putStrLn "  --restore ID         Restore service from snapshot"
  putStrLn "  --from SNAPSHOT_ID   Snapshot ID to restore from"
  putStrLn "  --snapshot-name NAME Optional snapshot name"
  putStrLn "  --hot                Take live snapshot without freezing"
  putStrLn ""
  putStrLn "Snapshot management options:"
  putStrLn "  -l, --list           List all snapshots"
  putStrLn "  --info ID            Get snapshot details"
  putStrLn "  --delete ID          Delete a snapshot"
  putStrLn "  --clone ID           Clone snapshot to new session/service"
  putStrLn "  --type TYPE          Type for clone (session|service)"
  putStrLn "  --name NAME          Name for cloned instance"
  putStrLn "  --ports PORTS        Ports for cloned service"
  putStrLn ""
  putStrLn "Key options:"
  putStrLn "  --extend        Open browser to extend/renew key"
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
  (exitCode, stdout, stderr) <- curlPost apiKey "https://api.unsandbox.com/execute" json

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
      (_, stdout, _) <- curlGet apiKey "https://api.unsandbox.com/sessions"
      putStrLn stdout
    SessionKill sid -> do
      (_, stdout, _) <- curlDelete apiKey ("https://api.unsandbox.com/sessions/" ++ sid)
      putStrLn $ green ++ "Session terminated: " ++ sid ++ reset
    SessionSnapshot sid -> do
      let nameJSON = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\",") (sessSnapshotName opts)
      let hotJSON = if sessHot opts then "\"hot\":true" else "\"hot\":false"
      let json = "{" ++ nameJSON ++ hotJSON ++ "}"
      hPutStrLn stderr $ "Creating snapshot of session " ++ sid ++ "..."
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/sessions/" ++ sid ++ "/snapshot") json
      putStrLn $ green ++ "Snapshot created" ++ reset
      putStrLn stdout
    SessionRestore snapshotId -> do
      -- --restore takes snapshot ID directly, calls /snapshots/:id/restore
      hPutStrLn stderr $ "Restoring from snapshot " ++ snapshotId ++ "..."
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/snapshots/" ++ snapshotId ++ "/restore") "{}"
      putStrLn $ green ++ "Session restored from snapshot" ++ reset
      putStrLn stdout
    SessionCreate -> do
      let shell = maybe "bash" id (sessShell opts)
      let networkJSON = maybe "" (\n -> ",\"network\":\"" ++ n ++ "\"") (sessNetwork opts)
      let vcpuJSON = maybe "" (\v -> ",\"vcpu\":" ++ show v) (sessVcpu opts)
      -- Input files
      filesJSON <- if null (sessFiles opts)
        then return ""
        else do
          fileEntries <- mapM (\f -> do
            content <- BS.readFile f
            let b64 = BSC.unpack $ B64.encode content
            let fname = takeFileName f
            return $ "{\"filename\":\"" ++ fname ++ "\",\"content_base64\":\"" ++ b64 ++ "\"}"
            ) (sessFiles opts)
          return $ ",\"input_files\":[" ++ intercalate "," fileEntries ++ "]"
      let json = "{\"shell\":\"" ++ shell ++ "\"" ++ networkJSON ++ vcpuJSON ++ filesJSON ++ "}"
      (_, stdout, _) <- curlPost apiKey "https://api.unsandbox.com/sessions" json
      putStrLn $ yellow ++ "Session created (WebSocket required for interactivity)" ++ reset
      putStrLn stdout

-- Service command
serviceCommand :: ServiceOpts -> IO ()
serviceCommand opts = do
  apiKey <- getApiKey
  case svcAction opts of
    ServiceList -> do
      (_, stdout, _) <- curlGet apiKey "https://api.unsandbox.com/services"
      putStrLn stdout
    ServiceInfo sid -> do
      (_, stdout, _) <- curlGet apiKey ("https://api.unsandbox.com/services/" ++ sid)
      putStrLn stdout
    ServiceLogs sid -> do
      (_, stdout, _) <- curlGet apiKey ("https://api.unsandbox.com/services/" ++ sid ++ "/logs")
      putStrLn stdout
    ServiceSleep sid -> do
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/services/" ++ sid ++ "/sleep") "{}"
      putStrLn $ green ++ "Service sleeping: " ++ sid ++ reset
    ServiceWake sid -> do
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/services/" ++ sid ++ "/wake") "{}"
      putStrLn $ green ++ "Service waking: " ++ sid ++ reset
    ServiceDestroy sid -> do
      (_, stdout, _) <- curlDelete apiKey ("https://api.unsandbox.com/services/" ++ sid)
      putStrLn $ green ++ "Service destroyed: " ++ sid ++ reset
    ServiceExecute sid cmd -> do
      let json = "{\"command\":\"" ++ escapeJSON cmd ++ "\"}"
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/services/" ++ sid ++ "/execute") json
      unless (null stdout) $ putStr $ blue ++ stdout ++ reset
    ServiceDumpBootstrap sid maybeFile -> do
      hPutStrLn stderr $ "Fetching bootstrap script from " ++ sid ++ "..."
      let json = "{\"command\":\"cat /tmp/bootstrap.sh\"}"
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/services/" ++ sid ++ "/execute") json
      -- Extract stdout from JSON response
      let bootstrapScript = extractJsonString stdout "stdout"
      case bootstrapScript of
        Just script | not (null script) -> do
          case maybeFile of
            Just file -> do
              writeFile file script
              perms <- getPermissions file
              setPermissions file (setOwnerExecutable True perms)
              putStrLn $ "Bootstrap saved to " ++ file
            Nothing -> putStr script
        _ -> do
          hPutStrLn stderr $ red ++ "Error: Failed to fetch bootstrap (service not running or no bootstrap file)" ++ reset
          exitFailure
    ServiceSnapshot sid -> do
      let nameJSON = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\",") (svcSnapshotName opts)
      let hotJSON = if svcHot opts then "\"hot\":true" else "\"hot\":false"
      let json = "{" ++ nameJSON ++ hotJSON ++ "}"
      hPutStrLn stderr $ "Creating snapshot of service " ++ sid ++ "..."
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/services/" ++ sid ++ "/snapshot") json
      putStrLn $ green ++ "Snapshot created" ++ reset
      putStrLn stdout
    ServiceRestore snapshotId -> do
      -- --restore takes snapshot ID directly, calls /snapshots/:id/restore
      hPutStrLn stderr $ "Restoring from snapshot " ++ snapshotId ++ "..."
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/snapshots/" ++ snapshotId ++ "/restore") "{}"
      putStrLn $ green ++ "Service restored from snapshot" ++ reset
      putStrLn stdout
    ServiceCreate -> do
      case svcName opts of
        Nothing -> do
          hPutStrLn stderr "Error: --name required to create service"
          exitFailure
        Just name -> do
          let portsJSON = maybe "" (\p -> ",\"ports\":[" ++ p ++ "]") (svcPorts opts)
          let typeJSON = maybe "" (\t -> ",\"service_type\":\"" ++ t ++ "\"") (svcType opts)
          let bootstrapJSON = maybe "" (\b -> ",\"bootstrap\":\"" ++ escapeJSON b ++ "\"") (svcBootstrap opts)
          bootstrapContentJSON <- case svcBootstrapFile opts of
            Just f -> do
              content <- readFile f
              return $ ",\"bootstrap_content\":\"" ++ escapeJSON content ++ "\""
            Nothing -> return ""
          let networkJSON = maybe "" (\n -> ",\"network\":\"" ++ n ++ "\"") (svcNetwork opts)
          let vcpuJSON = maybe "" (\v -> ",\"vcpu\":" ++ show v) (svcVcpu opts)
          -- Input files
          filesJSON <- if null (svcFiles opts)
            then return ""
            else do
              fileEntries <- mapM (\f -> do
                content <- BS.readFile f
                let b64 = BSC.unpack $ B64.encode content
                let fname = takeFileName f
                return $ "{\"filename\":\"" ++ fname ++ "\",\"content_base64\":\"" ++ b64 ++ "\"}"
                ) (svcFiles opts)
              return $ ",\"input_files\":[" ++ intercalate "," fileEntries ++ "]"
          let json = "{\"name\":\"" ++ name ++ "\"" ++ portsJSON ++ typeJSON ++ bootstrapJSON ++ bootstrapContentJSON ++ networkJSON ++ vcpuJSON ++ filesJSON ++ "}"
          (_, stdout, _) <- curlPost apiKey "https://api.unsandbox.com/services" json
          putStrLn $ green ++ "Service created" ++ reset
          putStrLn stdout

-- Check for clock drift error
checkClockDriftError :: String -> IO ()
checkClockDriftError response = do
  let hasTimestamp = "timestamp" `isPrefixOf` dropWhile (/= 't') response ||
                     "\"timestamp\"" `isInfixOf` response
  let has401 = "401" `isInfixOf` response
  let hasExpired = "expired" `isInfixOf` response
  let hasInvalid = "invalid" `isInfixOf` response

  when (hasTimestamp && (has401 || hasExpired || hasInvalid)) $ do
    hPutStrLn stderr $ red ++ "Error: Request timestamp expired (must be within 5 minutes of server time)" ++ reset
    hPutStrLn stderr $ yellow ++ "Your computer's clock may have drifted." ++ reset
    hPutStrLn stderr "Check your system time and sync with NTP if needed:"
    hPutStrLn stderr "  Linux:   sudo ntpdate -s time.nist.gov"
    hPutStrLn stderr "  macOS:   sudo sntp -sS time.apple.com"
    hPutStrLn stderr "  Windows: w32tm /resync"
    exitFailure
  where
    isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs

-- HTTP helpers using curl
curlPost :: String -> String -> String -> IO (ExitCode, String, String)
curlPost apiKey url body = do
  (publicKey, secretKey) <- getApiKeys
  -- Extract path from URL
  let path = drop (length "https://api.unsandbox.com") url
  authHeaders <- buildAuthHeaders publicKey secretKey "POST" path body
  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", "-X", "POST"
     , url
     , "-H", "Content-Type: application/json"
     ] ++ authHeaders ++ ["-d", body]) ""
  -- Check for clock drift error
  checkClockDriftError stdout
  return (exitCode, stdout, stderr)

curlGet :: String -> String -> IO (ExitCode, String, String)
curlGet apiKey url = do
  (publicKey, secretKey) <- getApiKeys
  let path = drop (length "https://api.unsandbox.com") url
  authHeaders <- buildAuthHeaders publicKey secretKey "GET" path ""
  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", url ] ++ authHeaders) ""
  -- Check for clock drift error
  checkClockDriftError stdout
  return (exitCode, stdout, stderr)

curlDelete :: String -> String -> IO (ExitCode, String, String)
curlDelete apiKey url = do
  (publicKey, secretKey) <- getApiKeys
  let path = drop (length "https://api.unsandbox.com") url
  authHeaders <- buildAuthHeaders publicKey secretKey "DELETE" path ""
  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", "-X", "DELETE", url ] ++ authHeaders) ""
  -- Check for clock drift error
  checkClockDriftError stdout
  return (exitCode, stdout, stderr)

-- Get API keys from environment
getApiKeys :: IO (String, Maybe String)
getApiKeys = do
  publicKey <- lookupEnv "UNSANDBOX_PUBLIC_KEY"
  secretKey <- lookupEnv "UNSANDBOX_SECRET_KEY"
  apiKey <- lookupEnv "UNSANDBOX_API_KEY"
  case (publicKey, secretKey, apiKey) of
    (Just pk, Just sk, _) -> return (pk, Just sk)
    (_, _, Just ak) -> return (ak, Nothing)
    _ -> do
      hPutStrLn stderr "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)"
      exitFailure

getApiKey :: IO String
getApiKey = do
  (publicKey, _) <- getApiKeys
  return publicKey

-- HMAC-SHA256
hmacSha256 :: String -> String -> String
hmacSha256 secret message =
  let secretBS = BSC.pack secret
      messageBS = BSC.pack message
      mac = hmac secretBS messageBS
  in concatMap (printf "%02x") (BS.unpack mac)

makeSignature :: String -> String -> String -> String -> String -> String
makeSignature secretKey timestamp method path body =
  let message = timestamp ++ ":" ++ method ++ ":" ++ path ++ ":" ++ body
  in hmacSha256 secretKey message

buildAuthHeaders :: String -> Maybe String -> String -> String -> String -> IO [String]
buildAuthHeaders publicKey maybeSecretKey method path body =
  case maybeSecretKey of
    Just secretKey -> do
      now <- getPOSIXTime
      let timestamp = show (floor now :: Integer)
      let signature = makeSignature secretKey timestamp method path body
      return [ "-H", "Authorization: Bearer " ++ publicKey
             , "-H", "X-Timestamp: " ++ timestamp
             , "-H", "X-Signature: " ++ signature
             ]
    Nothing ->
      return ["-H", "Authorization: Bearer " ++ publicKey]

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

-- Extract JSON string field (simple parser for basic cases)
extractJsonString :: String -> String -> Maybe String
extractJsonString json field =
  case break (== '"') rest of
    (_, '"':value) ->
      case break (== '"') value of
        (v, _) -> Just v
    _ -> Nothing
  where
    needle = "\"" ++ field ++ "\":"
    rest = case dropWhile (/= '"') $ dropWhile (not . isPrefixOf needle) $ tails json of
      (_:xs) -> case dropWhile (/= ':') xs of
        (_:ys) -> dropWhile (`elem` " \t\n") ys
        _ -> ""
      _ -> ""
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs

-- Snapshot command
snapshotCommand :: SnapshotOpts -> IO ()
snapshotCommand opts = do
  apiKey <- getApiKey
  case snapAction opts of
    SnapshotList -> do
      (_, stdout, _) <- curlGet apiKey "https://api.unsandbox.com/snapshots"
      putStrLn stdout
    SnapshotInfo sid -> do
      (_, stdout, _) <- curlGet apiKey ("https://api.unsandbox.com/snapshots/" ++ sid)
      putStrLn stdout
    SnapshotDelete sid -> do
      (_, stdout, _) <- curlDelete apiKey ("https://api.unsandbox.com/snapshots/" ++ sid)
      putStrLn $ green ++ "Snapshot deleted: " ++ sid ++ reset
    SnapshotClone sid -> do
      case snapCloneType opts of
        Nothing -> do
          hPutStrLn stderr "Error: --type (session|service) required for clone"
          exitFailure
        Just cloneType -> do
          let nameJSON = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\",") (snapCloneName opts)
          let portsJSON = maybe "" (\p -> "\"ports\":[" ++ p ++ "],") (snapClonePorts opts)
          let json = "{\"type\":\"" ++ cloneType ++ "\"," ++ nameJSON ++ portsJSON ++ "}"
          hPutStrLn stderr $ "Cloning snapshot " ++ sid ++ " to create new " ++ cloneType ++ "..."
          (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/snapshots/" ++ sid ++ "/clone") json
          putStrLn $ green ++ "Snapshot cloned" ++ reset
          putStrLn stdout

-- Key command
keyCommand :: KeyOpts -> IO ()
keyCommand opts = do
  apiKey <- getApiKey

  if keyExtend opts
    then extendKey apiKey
    else validateKey apiKey

-- Validate API key and show status
validateKey :: String -> IO ()
validateKey apiKey = do
  let url = portalBase ++ "/keys/validate"
  (exitCode, stdout, stderr) <- curlPostPortal apiKey url "{}"

  -- Check if valid:false appears in response
  let isInvalid = "\"valid\":false" `isPrefixOf` dropWhile (/= 'v') stdout

  if exitCode /= ExitSuccess || isInvalid
    then do
      -- Parse error response
      let reason = extractJsonString stdout "reason"
      case reason of
        Just "expired" -> do
          putStrLn $ red ++ "Expired" ++ reset ++ "\n"

          -- Show key details
          case extractJsonString stdout "public_key" of
            Just pk -> putStrLn $ "Public Key:          " ++ pk
            Nothing -> return ()

          case extractJsonString stdout "tier" of
            Just tier -> putStrLn $ "Tier:                " ++ tier
            Nothing -> return ()

          case extractJsonString stdout "expired_at_datetime" of
            Just expiredAt -> do
              putStr $ "Expired:             " ++ expiredAt
              case extractJsonString stdout "expired_ago" of
                Just ago -> putStrLn $ " (" ++ ago ++ ")"
                Nothing -> putStrLn ""
            Nothing -> return ()

          putStrLn ""
          putStrLn $ yellow ++ "To renew:" ++ reset ++ " Visit https://unsandbox.com/keys/extend"
          exitFailure

        Just "invalid_key" -> do
          putStrLn $ red ++ "Invalid" ++ reset ++ ": key not found"
          exitFailure

        Just "suspended" -> do
          putStrLn $ red ++ "Suspended" ++ reset ++ ": key has been suspended"
          exitFailure

        _ -> do
          putStrLn $ red ++ "Invalid key" ++ reset
          exitFailure
    else do
      -- Parse valid response
      putStrLn $ green ++ "Valid" ++ reset ++ "\n"

      case extractJsonString stdout "public_key" of
        Just pk -> putStrLn $ "Public Key:          " ++ pk
        Nothing -> return ()

      case extractJsonString stdout "tier" of
        Just tier -> putStrLn $ "Tier:                " ++ tier
        Nothing -> return ()

      case extractJsonString stdout "status" of
        Just status -> putStrLn $ "Status:              " ++ status
        Nothing -> return ()

      case extractJsonString stdout "valid_through_datetime" of
        Just validThrough -> putStrLn $ "Expires:             " ++ validThrough
        Nothing -> return ()

      case extractJsonString stdout "valid_for_human" of
        Just validFor -> putStrLn $ "Time Remaining:      " ++ validFor
        Nothing -> return ()

      case extractJsonString stdout "rate_per_minute" of
        Just rate -> putStrLn $ "Rate Limit:          " ++ rate ++ "/min"
        Nothing -> return ()

      case extractJsonString stdout "burst" of
        Just burst -> putStrLn $ "Burst:               " ++ burst
        Nothing -> return ()

      case extractJsonString stdout "concurrency" of
        Just conc -> putStrLn $ "Concurrency:         " ++ conc
        Nothing -> return ()

-- Extend key (open browser to extend page)
extendKey :: String -> IO ()
extendKey apiKey = do
  let url = portalBase ++ "/keys/validate"
  (exitCode, stdout, _) <- curlPostPortal apiKey url "{}"

  case extractJsonString stdout "public_key" of
    Nothing -> do
      hPutStrLn stderr "Error: Invalid key or could not retrieve public key"
      exitFailure
    Just publicKey -> do
      let extendUrl = portalBase ++ "/keys/extend?pk=" ++ publicKey
      putStrLn "Opening extension page in browser..."
      putStrLn $ "If browser doesn't open, visit: " ++ extendUrl

      -- Try to open URL in browser (Linux-specific)
      _ <- readProcessWithExitCode "sh"
        ["-c", "xdg-open '" ++ extendUrl ++ "' 2>/dev/null || sensible-browser '" ++ extendUrl ++ "' 2>/dev/null || true"]
        ""
      return ()

-- HTTP helper for portal API
curlPostPortal :: String -> String -> String -> IO (ExitCode, String, String)
curlPostPortal apiKey url body = do
  (publicKey, secretKey) <- getApiKeys
  let path = drop (length portalBase) url
  authHeaders <- buildAuthHeaders publicKey secretKey "POST" path body
  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", "-X", "POST"
     , url
     , "-H", "Content-Type: application/json"
     ] ++ authHeaders ++ ["-d", body]) ""
  -- Check for clock drift error
  checkClockDriftError stdout
  return (exitCode, stdout, stderr)
