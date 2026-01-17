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
import Control.Exception (try, catch, IOError)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64
import Crypto.Hash.SHA256 (hmac)
import Numeric (showHex)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (diffUTCTime)
import qualified Data.Time.Clock
import qualified System.Posix.Files

-- API constants
apiBase :: String
apiBase = "https://api.unsandbox.com"

portalBase :: String
portalBase = "https://unsandbox.com"

languagesCacheTtl :: Int
languagesCacheTtl = 3600  -- 1 hour in seconds

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
data Command = Execute ExecuteOpts | Session SessionOpts | Service ServiceOpts | Key KeyOpts | Snapshot SnapshotOpts | Image ImageOpts | Languages LanguagesOpts | Help

data LanguagesOpts = LanguagesOpts
  { langJson :: Bool
  }

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
  , svcEnvs :: [(String, String)]
  , svcEnvFile :: Maybe String
  }

data ServiceAction = ServiceList | ServiceInfo String | ServiceLogs String
                   | ServiceSleep String | ServiceWake String | ServiceDestroy String
                   | ServiceResize String | ServiceExecute String String | ServiceDumpBootstrap String (Maybe String)
                   | ServiceCreate | ServiceSnapshot String | ServiceRestore String
                   | ServiceEnv String (Maybe String)  -- action, target

data SnapshotOpts = SnapshotOpts
  { snapAction :: SnapshotAction
  , snapCloneType :: Maybe String
  , snapCloneName :: Maybe String
  , snapClonePorts :: Maybe String
  }

data SnapshotAction = SnapshotList | SnapshotInfo String | SnapshotDelete String
                    | SnapshotClone String

data ImageOpts = ImageOpts
  { imgAction :: ImageAction
  , imgName :: Maybe String
  , imgPorts :: Maybe String
  , imgSourceType :: Maybe String
  , imgVisibilityMode :: Maybe String
  }

data ImageAction = ImageList | ImageInfo String | ImageDelete String
                 | ImageLock String | ImageUnlock String | ImagePublish String
                 | ImageVisibility String | ImageSpawn String | ImageClone String

data KeyOpts = KeyOpts
  { keyExtend :: Bool
  }

-- Parse arguments
parseArgs :: [String] -> IO Command
parseArgs ("session":rest) = Session <$> parseSession rest
parseArgs ("service":rest) = Service <$> parseService rest
parseArgs ("key":rest) = Key <$> parseKey rest
parseArgs ("snapshot":rest) = Snapshot <$> parseSnapshot rest
parseArgs ("image":rest) = Image <$> parseImage rest
parseArgs ("languages":rest) = Languages <$> parseLanguages rest
parseArgs args = parseExecute args

parseLanguages :: [String] -> IO LanguagesOpts
parseLanguages args = return $ parseLanguagesArgs args defaultLanguagesOpts
  where
    defaultLanguagesOpts = LanguagesOpts False
    parseLanguagesArgs [] opts = opts
    parseLanguagesArgs ("--json":rest) opts = parseLanguagesArgs rest opts { langJson = True }
    parseLanguagesArgs (_:rest) opts = parseLanguagesArgs rest opts

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

parseImage :: [String] -> IO ImageOpts
parseImage args = return $ parseImageArgs args defaultImageOpts
  where
    defaultImageOpts = ImageOpts ImageList Nothing Nothing Nothing Nothing
    parseImageArgs [] opts = opts
    parseImageArgs ("--list":rest) opts = parseImageArgs rest opts { imgAction = ImageList }
    parseImageArgs ("-l":rest) opts = parseImageArgs rest opts { imgAction = ImageList }
    parseImageArgs ("--info":id:rest) opts = parseImageArgs rest opts { imgAction = ImageInfo id }
    parseImageArgs ("--delete":id:rest) opts = parseImageArgs rest opts { imgAction = ImageDelete id }
    parseImageArgs ("--lock":id:rest) opts = parseImageArgs rest opts { imgAction = ImageLock id }
    parseImageArgs ("--unlock":id:rest) opts = parseImageArgs rest opts { imgAction = ImageUnlock id }
    parseImageArgs ("--publish":id:rest) opts = parseImageArgs rest opts { imgAction = ImagePublish id }
    parseImageArgs ("--source-type":t:rest) opts = parseImageArgs rest opts { imgSourceType = Just t }
    parseImageArgs ("--visibility":id:mode:rest) opts = parseImageArgs rest opts { imgAction = ImageVisibility id, imgVisibilityMode = Just mode }
    parseImageArgs ("--spawn":id:rest) opts = parseImageArgs rest opts { imgAction = ImageSpawn id }
    parseImageArgs ("--clone":id:rest) opts = parseImageArgs rest opts { imgAction = ImageClone id }
    parseImageArgs ("--name":n:rest) opts = parseImageArgs rest opts { imgName = Just n }
    parseImageArgs ("--ports":p:rest) opts = parseImageArgs rest opts { imgPorts = Just p }
    parseImageArgs (_:rest) opts = parseImageArgs rest opts

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
    defaultServiceOpts = ServiceOpts ServiceCreate Nothing Nothing Nothing Nothing Nothing Nothing Nothing [] Nothing Nothing False [] Nothing
    parseServiceArgs [] opts = opts
    parseServiceArgs ("--list":rest) opts = parseServiceArgs rest opts { svcAction = ServiceList }
    parseServiceArgs ("--info":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceInfo id }
    parseServiceArgs ("--logs":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceLogs id }
    parseServiceArgs ("--freeze":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceSleep id }
    parseServiceArgs ("--unfreeze":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceWake id }
    parseServiceArgs ("--destroy":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceDestroy id }
    parseServiceArgs ("--resize":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceResize id }
    parseServiceArgs ("--snapshot":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceSnapshot id }
    parseServiceArgs ("--restore":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceRestore id }
    parseServiceArgs ("--execute":id:"--command":cmd:rest) opts = parseServiceArgs rest opts { svcAction = ServiceExecute id cmd }
    parseServiceArgs ("--dump-bootstrap":id:file:rest) opts = parseServiceArgs rest opts { svcAction = ServiceDumpBootstrap id (Just file) }
    parseServiceArgs ("--dump-bootstrap":id:rest) opts = parseServiceArgs rest opts { svcAction = ServiceDumpBootstrap id Nothing }
    parseServiceArgs ("env":action:target:rest) opts = parseServiceArgs rest opts { svcAction = ServiceEnv action (Just target) }
    parseServiceArgs ("env":action:rest) opts = parseServiceArgs rest opts { svcAction = ServiceEnv action Nothing }
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
    parseServiceArgs ("-e":kv:rest) opts =
      let (k, v) = span (/= '=') kv
      in parseServiceArgs rest opts { svcEnvs = svcEnvs opts ++ [(k, drop 1 v)] }
    parseServiceArgs ("--env-file":f:rest) opts = parseServiceArgs rest opts { svcEnvFile = Just f }
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
    Image opts -> imageCommand opts
    Languages opts -> languagesCommand opts
    Help -> printHelp

printHelp :: IO ()
printHelp = do
  putStrLn "Usage:"
  putStrLn "  un.hs [options] <source_file>         Execute code"
  putStrLn "  un.hs session [options]                Manage sessions"
  putStrLn "  un.hs service [options]                Manage services"
  putStrLn "  un.hs service env <action> <id>        Manage service vault"
  putStrLn "  un.hs snapshot [options]               Manage snapshots"
  putStrLn "  un.hs image [options]                  Manage images"
  putStrLn "  un.hs languages [--json]               List available languages"
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
  putStrLn "Service options:"
  putStrLn "  -e KEY=VALUE         Set vault env var (with --name or env set)"
  putStrLn "  --env-file FILE      Load vault vars from file"
  putStrLn "  --freeze ID          Freeze service"
  putStrLn "  --unfreeze ID        Unfreeze service"
  putStrLn "  --destroy ID         Destroy service"
  putStrLn "  --resize ID          Resize service (requires -v N)"
  putStrLn ""
  putStrLn "Service env commands:"
  putStrLn "  env status ID        Check vault status"
  putStrLn "  env set ID           Set vault (use -e or --env-file)"
  putStrLn "  env export ID        Export vault contents"
  putStrLn "  env delete ID        Delete vault"
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
  putStrLn ""
  putStrLn "Image options:"
  putStrLn "  -l, --list           List all images"
  putStrLn "  --info ID            Get image details"
  putStrLn "  --delete ID          Delete an image"
  putStrLn "  --lock ID            Lock image to prevent deletion"
  putStrLn "  --unlock ID          Unlock image"
  putStrLn "  --publish ID         Publish image from service/snapshot (requires --source-type)"
  putStrLn "  --source-type TYPE   Source type: service or snapshot"
  putStrLn "  --visibility ID MODE Set visibility: private, unlisted, or public"
  putStrLn "  --spawn ID           Spawn new service from image"
  putStrLn "  --clone ID           Clone an image"
  putStrLn "  --name NAME          Name for spawned service or cloned image"
  putStrLn "  --ports PORTS        Ports for spawned service"
  putStrLn ""
  putStrLn "Languages options:"
  putStrLn "  --json          Output as JSON array"
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
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/services/" ++ sid ++ "/freeze") "{}"
      putStrLn $ green ++ "Service frozen: " ++ sid ++ reset
    ServiceWake sid -> do
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/services/" ++ sid ++ "/unfreeze") "{}"
      putStrLn $ green ++ "Service unfreezing: " ++ sid ++ reset
    ServiceDestroy sid -> do
      (_, stdout, _) <- curlDelete apiKey ("https://api.unsandbox.com/services/" ++ sid)
      putStrLn $ green ++ "Service destroyed: " ++ sid ++ reset
    ServiceResize sid -> do
      case svcVcpu opts of
        Nothing -> do
          hPutStrLn stderr $ red ++ "Error: --resize requires -v N (1-8)" ++ reset
          exitFailure
        Just vcpu -> do
          let json = "{\"vcpu\":" ++ show vcpu ++ "}"
          let ram = vcpu * 2
          (_, stdout, _) <- curlPatch apiKey ("https://api.unsandbox.com/services/" ++ sid) json
          putStrLn $ green ++ "Service resized to " ++ show vcpu ++ " vCPU, " ++ show ram ++ " GB RAM" ++ reset
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
    ServiceEnv action maybeTarget -> do
      case action of
        "status" -> case maybeTarget of
          Just target -> do
            result <- serviceEnvStatus target
            let hasVault = "\"has_vault\":true" `isPrefixOf` dropWhile (/= 'h') result
            if hasVault
              then do
                putStrLn $ green ++ "Vault: configured" ++ reset
                case extractJsonString result "env_count" of
                  Just count -> putStrLn $ "Variables: " ++ count
                  Nothing -> return ()
                case extractJsonString result "updated_at" of
                  Just updated -> putStrLn $ "Updated: " ++ updated
                  Nothing -> return ()
              else putStrLn $ yellow ++ "Vault: not configured" ++ reset
          Nothing -> do
            hPutStrLn stderr $ red ++ "Error: service env status requires service ID" ++ reset
            exitFailure
        "set" -> case maybeTarget of
          Just target -> do
            if null (svcEnvs opts) && svcEnvFile opts == Nothing
              then do
                hPutStrLn stderr $ red ++ "Error: service env set requires -e or --env-file" ++ reset
                exitFailure
              else do
                envContent <- buildEnvContent (svcEnvs opts) (svcEnvFile opts)
                success <- serviceEnvSet target envContent
                if success
                  then putStrLn $ green ++ "Vault updated for service " ++ target ++ reset
                  else do
                    hPutStrLn stderr $ red ++ "Error: Failed to update vault" ++ reset
                    exitFailure
          Nothing -> do
            hPutStrLn stderr $ red ++ "Error: service env set requires service ID" ++ reset
            exitFailure
        "export" -> case maybeTarget of
          Just target -> do
            result <- serviceEnvExport target
            case extractJsonString result "content" of
              Just content -> putStr content
              Nothing -> return ()
          Nothing -> do
            hPutStrLn stderr $ red ++ "Error: service env export requires service ID" ++ reset
            exitFailure
        "delete" -> case maybeTarget of
          Just target -> do
            success <- serviceEnvDelete target
            if success
              then putStrLn $ green ++ "Vault deleted for service " ++ target ++ reset
              else do
                hPutStrLn stderr $ red ++ "Error: Failed to delete vault" ++ reset
                exitFailure
          Nothing -> do
            hPutStrLn stderr $ red ++ "Error: service env delete requires service ID" ++ reset
            exitFailure
        _ -> do
          hPutStrLn stderr $ red ++ "Error: Unknown env action: " ++ action ++ reset
          hPutStrLn stderr "Usage: un.hs service env <status|set|export|delete> <service_id>"
          exitFailure
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

          -- Auto-set vault if env vars were provided
          when (not (null (svcEnvs opts)) || svcEnvFile opts /= Nothing) $ do
            case extractJsonString stdout "id" of
              Just serviceId -> do
                envContent <- buildEnvContent (svcEnvs opts) (svcEnvFile opts)
                when (not (null envContent)) $ do
                  success <- serviceEnvSet serviceId envContent
                  if success
                    then putStrLn $ green ++ "Vault configured with environment variables" ++ reset
                    else hPutStrLn stderr $ yellow ++ "Warning: Failed to set vault" ++ reset
              Nothing -> return ()

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

curlPatch :: String -> String -> String -> IO (ExitCode, String, String)
curlPatch apiKey url body = do
  (publicKey, secretKey) <- getApiKeys
  let path = drop (length "https://api.unsandbox.com") url
  authHeaders <- buildAuthHeaders publicKey secretKey "PATCH" path body
  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", "-X", "PATCH"
     , url
     , "-H", "Content-Type: application/json"
     ] ++ authHeaders ++ ["-d", body]) ""
  checkClockDriftError stdout
  return (exitCode, stdout, stderr)

curlPut :: String -> String -> String -> IO (ExitCode, String, String)
curlPut apiKey url body = do
  (publicKey, secretKey) <- getApiKeys
  let path = drop (length "https://api.unsandbox.com") url
  authHeaders <- buildAuthHeaders publicKey secretKey "PUT" path body
  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", "-X", "PUT"
     , url
     , "-H", "Content-Type: text/plain"
     ] ++ authHeaders ++ ["-d", body]) ""
  checkClockDriftError stdout
  return (exitCode, stdout, stderr)

-- Vault helper functions
maxEnvContentSize :: Int
maxEnvContentSize = 65536

readEnvFile :: String -> IO String
readEnvFile path = do
  content <- readFile path
  return content

buildEnvContent :: [(String, String)] -> Maybe String -> IO String
buildEnvContent envs maybeEnvFile = do
  -- Add from -e flags
  let envLines = map (\(k, v) -> k ++ "=" ++ v) envs

  -- Add from --env-file
  fileLines <- case maybeEnvFile of
    Just path -> do
      content <- readEnvFile path
      return $ filter (not . null) $ filter (not . isPrefixOf "#") $ map (filter (/= '\r')) $ lines content
    Nothing -> return []

  return $ intercalate "\n" (envLines ++ fileLines)

serviceEnvStatus :: String -> IO String
serviceEnvStatus serviceId = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlGet apiKey (apiBase ++ "/services/" ++ serviceId ++ "/env")
  return stdout

serviceEnvSet :: String -> String -> IO Bool
serviceEnvSet serviceId envContent = do
  if length envContent > maxEnvContentSize
    then do
      hPutStrLn stderr $ red ++ "Error: Env content exceeds maximum size of 64KB" ++ reset
      return False
    else do
      apiKey <- getApiKey
      (exitCode, _, _) <- curlPut apiKey (apiBase ++ "/services/" ++ serviceId ++ "/env") envContent
      return (exitCode == ExitSuccess)

serviceEnvExport :: String -> IO String
serviceEnvExport serviceId = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/services/" ++ serviceId ++ "/env/export") "{}"
  return stdout

serviceEnvDelete :: String -> IO Bool
serviceEnvDelete serviceId = do
  apiKey <- getApiKey
  (exitCode, _, _) <- curlDelete apiKey (apiBase ++ "/services/" ++ serviceId ++ "/env")
  return (exitCode == ExitSuccess)

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

-- Image command
imageCommand :: ImageOpts -> IO ()
imageCommand opts = do
  apiKey <- getApiKey
  case imgAction opts of
    ImageList -> do
      (_, stdout, _) <- curlGet apiKey "https://api.unsandbox.com/images"
      putStrLn stdout
    ImageInfo iid -> do
      (_, stdout, _) <- curlGet apiKey ("https://api.unsandbox.com/images/" ++ iid)
      putStrLn stdout
    ImageDelete iid -> do
      (_, stdout, _) <- curlDelete apiKey ("https://api.unsandbox.com/images/" ++ iid)
      putStrLn $ green ++ "Image deleted: " ++ iid ++ reset
    ImageLock iid -> do
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/images/" ++ iid ++ "/lock") "{}"
      putStrLn $ green ++ "Image locked: " ++ iid ++ reset
    ImageUnlock iid -> do
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/images/" ++ iid ++ "/unlock") "{}"
      putStrLn $ green ++ "Image unlocked: " ++ iid ++ reset
    ImagePublish sourceId -> do
      case imgSourceType opts of
        Nothing -> do
          hPutStrLn stderr $ red ++ "Error: --source-type required (service or snapshot)" ++ reset
          exitFailure
        Just sourceType -> do
          let nameJSON = maybe "" (\n -> ",\"name\":\"" ++ escapeJSON n ++ "\"") (imgName opts)
          let json = "{\"source_type\":\"" ++ sourceType ++ "\",\"source_id\":\"" ++ sourceId ++ "\"" ++ nameJSON ++ "}"
          (_, stdout, _) <- curlPost apiKey "https://api.unsandbox.com/images/publish" json
          putStrLn $ green ++ "Image published" ++ reset
          putStrLn stdout
    ImageVisibility iid -> do
      case imgVisibilityMode opts of
        Nothing -> do
          hPutStrLn stderr $ red ++ "Error: --visibility requires MODE (private, unlisted, or public)" ++ reset
          exitFailure
        Just mode -> do
          let json = "{\"visibility\":\"" ++ mode ++ "\"}"
          (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/images/" ++ iid ++ "/visibility") json
          putStrLn $ green ++ "Image visibility set to " ++ mode ++ ": " ++ iid ++ reset
    ImageSpawn iid -> do
      let nameJSON = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\"") (imgName opts)
      let portsJSON = maybe "" (\p -> ",\"ports\":[" ++ p ++ "]") (imgPorts opts)
      let json = if null nameJSON then "{" ++ (if null portsJSON then "" else drop 1 portsJSON) ++ "}"
                 else "{" ++ nameJSON ++ portsJSON ++ "}"
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/images/" ++ iid ++ "/spawn") json
      putStrLn $ green ++ "Service spawned from image" ++ reset
      putStrLn stdout
    ImageClone iid -> do
      let nameJSON = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\"") (imgName opts)
      let json = "{" ++ nameJSON ++ "}"
      (_, stdout, _) <- curlPost apiKey ("https://api.unsandbox.com/images/" ++ iid ++ "/clone") json
      putStrLn $ green ++ "Image cloned" ++ reset
      putStrLn stdout

-- Languages cache functions
getLanguagesCachePath :: IO FilePath
getLanguagesCachePath = do
  home <- lookupEnv "HOME"
  let homeDir = maybe "." id home
  return $ homeDir ++ "/.unsandbox/languages.json"

loadLanguagesCache :: IO (Maybe [String])
loadLanguagesCache = do
  cachePath <- getLanguagesCachePath
  exists <- doesFileExist cachePath
  if not exists
    then return Nothing
    else do
      -- Check if cache is fresh (< 1 hour old)
      modTime <- getModificationTime cachePath
      now <- getCurrentTime
      let ageSeconds = floor $ diffUTCTime now modTime
      if ageSeconds >= languagesCacheTtl
        then return Nothing
        else do
          content <- readFile cachePath
          return $ extractJsonArray content "languages"
  where
    doesFileExist path = do
      result <- try (readFile path) :: IO (Either IOError String)
      case result of
        Left _ -> return False
        Right _ -> return True
    getModificationTime path = do
      status <- System.Posix.Files.getFileStatus path
      return $ posixSecondsToUTCTime $ realToFrac $ modificationTime status
    getCurrentTime = Data.Time.Clock.getCurrentTime

saveLanguagesCache :: [String] -> IO ()
saveLanguagesCache languages = do
  cachePath <- getLanguagesCachePath
  let cacheDir = takeDirectory cachePath
  -- Create directory if needed
  createDirectoryIfMissing True cacheDir
  now <- getPOSIXTime
  let timestamp = show (floor now :: Integer)
  let langsJson = "[" ++ intercalate "," (map (\l -> "\"" ++ l ++ "\"") languages) ++ "]"
  let content = "{\"languages\":" ++ langsJson ++ ",\"timestamp\":" ++ timestamp ++ "}"
  writeFile cachePath content
  `catch` (\(_ :: IOError) -> return ())  -- Cache failures are non-fatal
  where
    takeDirectory path = reverse $ dropWhile (/= '/') $ reverse path

-- Languages command
languagesCommand :: LanguagesOpts -> IO ()
languagesCommand opts = do
  apiKey <- getApiKey

  -- Try cache first
  cached <- loadLanguagesCache
  langs <- case cached of
    Just languages -> return languages
    Nothing -> do
      (_, stdout, _) <- curlGet apiKey "https://api.unsandbox.com/languages"
      let languages = maybe [] id (extractJsonArray stdout "languages")
      -- Save to cache
      when (not (null languages)) $ saveLanguagesCache languages
      return languages

  let jsonOutput = langJson opts
  if jsonOutput
    then do
      -- Extract languages array and print as JSON
      putStrLn $ "[" ++ intercalate "," (map (\l -> "\"" ++ l ++ "\"") langs) ++ "]"
    else do
      -- Print each language on its own line
      mapM_ putStrLn langs

-- Extract JSON array of strings from response
extractJsonArray :: String -> String -> Maybe [String]
extractJsonArray json field =
  let needle = "\"" ++ field ++ "\":["
      rest = dropWhile (not . isPrefixOf needle) (tails json)
  in case rest of
       (x:_) -> Just $ parseArrayItems $ drop (length needle) x
       _ -> Nothing
  where
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs

    parseArrayItems :: String -> [String]
    parseArrayItems s = go (dropWhile (`elem` " \t\n") s) []
      where
        go (']':_) acc = reverse acc
        go ('"':rest) acc =
          let (item, remaining) = span (/= '"') rest
          in go (dropWhile (`elem` ",] \t\n") (drop 1 remaining)) (item : acc)
        go (_:rest) acc = go rest acc
        go [] acc = reverse acc

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
