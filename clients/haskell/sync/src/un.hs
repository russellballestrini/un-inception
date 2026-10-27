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
import System.IO (hPutStrLn, hPutStr, hFlush, stderr, stdout, stdin, hIsTerminalDevice)
import System.Directory (createDirectoryIfMissing, setPermissions, getPermissions, setOwnerExecutable)
import Data.List (isPrefixOf, intercalate)
import Data.Char (isDigit, ord, toLower)
import Text.Printf (printf)
import Control.Monad (when, unless, forM_)
import Control.Exception (try, catch)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import System.IO.Unsafe (unsafePerformIO)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as B64
import Crypto.MAC.HMAC (HMAC, hmac, hmacGetDigest)
import Crypto.Hash (SHA256(..), hashWith)
import Numeric (showHex)
import Data.Time.Clock.POSIX (getPOSIXTime, posixSecondsToUTCTime)
import Data.Time.Clock (diffUTCTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import qualified Data.Time.Clock
import qualified System.Posix.Files
import qualified System.Posix.Process
import System.FilePath (takeDirectory, (</>))

-- API constants
-- | API base URL: UNSANDBOX_API_URL (default https://api.unsandbox.com), no trailing slash
defaultApiBase :: String
defaultApiBase = "https://api.unsandbox.com"

{-# NOINLINE apiBase #-}
apiBase :: String
apiBase = unsafePerformIO $ do
  v <- lookupEnv "UNSANDBOX_API_URL"
  let base = maybe "" id v
  return $ if null base then defaultApiBase else reverse (dropWhile (== '/') (reverse base))

portalBase :: String
portalBase = "https://unsandbox.com"

languagesCacheTtl :: Int
languagesCacheTtl = 3600  -- 1 hour in seconds

-- Global account index set by --account N flag (Nothing = not set)
{-# NOINLINE cliAccountIndex #-}
cliAccountIndex :: IORef (Maybe Int)
cliAccountIndex = unsafePerformIO (newIORef Nothing)

-- Global target OS set by --os OS flag (Nothing = not set, API default linux)
{-# NOINLINE cliOs #-}
cliOs :: IORef (Maybe String)
cliOs = unsafePerformIO (newIORef Nothing)

-- | Validate a target OS: linux or freebsd, case-insensitive, normalized to lowercase
normalizeOs :: String -> Either String String
normalizeOs v =
  let lower = map toLower v
  in if lower `elem` ["linux", "freebsd"]
       then Right lower
       else Left ("invalid os '" ++ v ++ "' (expected linux or freebsd)")

-- | JSON fragment for "os" in a request body, empty when no OS given.
-- Fails (IO error) on an invalid OS, before any request is made.
osJsonFor :: Maybe String -> IO String
osJsonFor Nothing = return ""
osJsonFor (Just v) = case normalizeOs v of
  Right o -> return $ ",\"os\":\"" ++ o ++ "\""
  Left err -> ioError (userError err)

-- | OS JSON fragment from the global --os flag
cliOsJson :: IO String
cliOsJson = readIORef cliOs >>= osJsonFor

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
                   | ServiceSetUnfreezeOnDemand String Bool  -- service_id, enabled

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
  , keyRotate :: Bool
  , keyOtp :: Maybe String
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
    defaultKeyOpts = KeyOpts False False Nothing
    parseKeyArgs [] opts = opts
    parseKeyArgs ("--extend":rest) opts = parseKeyArgs rest opts { keyExtend = True }
    parseKeyArgs ("--rotate":rest) opts = parseKeyArgs rest opts { keyRotate = True }
    parseKeyArgs ("--otp":code:rest) opts = parseKeyArgs rest opts { keyOtp = Just code }
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
    parseServiceArgs ("--unfreeze-on-demand":id:"true":rest) opts = parseServiceArgs rest opts { svcAction = ServiceSetUnfreezeOnDemand id True }
    parseServiceArgs ("--unfreeze-on-demand":id:"false":rest) opts = parseServiceArgs rest opts { svcAction = ServiceSetUnfreezeOnDemand id False }
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
    Right (Just opts) -> return $ Execute opts
    Right Nothing -> return Help
    Left arg -> do
      hPutStrLn stderr $ red ++ "Unknown option: " ++ arg ++ reset
      exitFailure
  where
    defaultExecOpts = ExecuteOpts "" [] [] False Nothing Nothing Nothing
    parseExecArgs [] opts = Right (if null (exFile opts) then Nothing else Just opts)
    parseExecArgs (arg:rest) opts
      | "-e" `isPrefixOf` arg = parseExecArgs rest opts { exEnv = parseEnv rest : exEnv opts }
      | "-f" `isPrefixOf` arg = parseExecArgs rest opts { exFiles = head rest : exFiles opts }
      | "-a" == arg = parseExecArgs rest opts { exArtifacts = True }
      | "-o" `isPrefixOf` arg = parseExecArgs rest opts { exOutDir = Just (head rest) }
      | "-n" `isPrefixOf` arg = parseExecArgs rest opts { exNetwork = Just (head rest) }
      | "-v" `isPrefixOf` arg = parseExecArgs rest opts { exVcpu = Just (read (head rest)) }
      | "-" `isPrefixOf` arg = Left arg
      | null (exFile opts) = parseExecArgs rest opts { exFile = arg }
      | otherwise = parseExecArgs rest opts
    parseEnv (kv:rest) =
      let (k, v) = span (/= '=') kv
      in (k, drop 1 v)

-- ============================================================================
-- Library API
-- ============================================================================

-- SDK Version
sdkVersion :: String
sdkVersion = "4.2.0"

-- | Return the SDK version
version :: String
version = sdkVersion

-- | Check API health
healthCheck :: IO Bool
healthCheck = do
  (exitCode, stdout, _) <- readProcessWithExitCode "curl"
    ["-s", "-o", "/dev/null", "-w", "%{http_code}", apiBase ++ "/health"] ""
  return $ filter isDigit stdout == "200"

-- | Generate HMAC-SHA256 signature for a message
hmacSign :: String -> String -> String
hmacSign = hmacSha256

-- | Detect language from filename extension
detectLanguage :: String -> Maybe String
detectLanguage filename = extToLang (takeExtension filename)

-- | Get list of supported languages (list of strings)
getLanguages :: IO [String]
getLanguages = do
  cached <- loadLanguagesCache
  case cached of
    Just languages -> return languages
    Nothing -> do
      apiKey <- getApiKey
      (_, stdout, _) <- curlGet apiKey (apiBase ++ "/languages")
      let languages = maybe [] id (extractJsonArray stdout "languages")
      when (not (null languages)) $ saveLanguagesCache languages
      return languages

-- | Execute code synchronously
execute :: String -> String -> IO (Either String (Bool, String, String, Int))
execute = executeWithOs Nothing

-- | Execute code synchronously on a target OS (Just "linux" | Just "freebsd", Nothing = API default)
executeWithOs :: Maybe String -> String -> String -> IO (Either String (Bool, String, String, Int))
executeWithOs targetOs language code = do
  osJson <- osJsonFor targetOs
  apiKey <- getApiKey
  let json = "{\"language\":\"" ++ escapeJSON language ++ "\",\"code\":\"" ++ escapeJSON code ++ "\"" ++ osJson ++ "}"
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/execute") json
  case exitCode of
    ExitSuccess ->
      let success = case extractJsonString stdout "exit_code" of
            Just "0" -> True
            _ -> False
          stdoutVal = maybe "" id (extractJsonString stdout "stdout")
          stderrVal = maybe "" id (extractJsonString stdout "stderr")
          exitCodeVal = case extractJsonString stdout "exit_code" of
            Just s -> read (filter isDigit s) :: Int
            _ -> 0
      in return $ Right (success, stdoutVal, stderrVal, exitCodeVal)
    _ -> return $ Left "Execution failed"

-- | Execute code asynchronously, returning a job ID
executeAsync :: String -> String -> IO (Maybe String)
executeAsync = executeAsyncWithOs Nothing

-- | Execute code asynchronously on a target OS, returning a job ID
executeAsyncWithOs :: Maybe String -> String -> String -> IO (Maybe String)
executeAsyncWithOs targetOs language code = do
  osJson <- osJsonFor targetOs
  apiKey <- getApiKey
  let json = "{\"language\":\"" ++ escapeJSON language ++ "\",\"code\":\"" ++ escapeJSON code ++ "\"" ++ osJson ++ "}"
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/execute/async") json
  case exitCode of
    ExitSuccess -> return $ extractJsonString stdout "job_id"
    _ -> return Nothing

-- | Get job status
getJob :: String -> IO (Maybe (String, String))
getJob jobId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlGet apiKey (apiBase ++ "/jobs/" ++ jobId)
  case exitCode of
    ExitSuccess ->
      let status = maybe "unknown" id (extractJsonString stdout "status")
          language = maybe "" id (extractJsonString stdout "language")
      in return $ Just (status, language)
    _ -> return Nothing

-- | Wait for job completion
waitJob :: String -> IO (Either String (Bool, String, String, Int))
waitJob jobId = waitJobLoop jobId 0 100
  where
    pollDelays = [300, 450, 700, 900, 650, 1600, 2000]
    terminalStates = ["completed", "failed", "timeout", "cancelled"]

    waitJobLoop jid pollCount maxPolls
      | pollCount >= maxPolls = return $ Left "Max polls exceeded"
      | otherwise = do
          let delayIdx = min pollCount (length pollDelays - 1)
          let delayMs = pollDelays !! delayIdx
          threadDelay (delayMs * 1000)  -- threadDelay takes microseconds

          result <- getJob jid
          case result of
            Just (status, _) | status `elem` terminalStates -> do
              apiKey <- getApiKey
              (_, stdout, _) <- curlGet apiKey (apiBase ++ "/jobs/" ++ jid)
              let success = case extractJsonString stdout "exit_code" of
                    Just "0" -> True
                    _ -> False
                  stdoutVal = maybe "" id (extractJsonString stdout "stdout")
                  stderrVal = maybe "" id (extractJsonString stdout "stderr")
                  exitCodeVal = case extractJsonString stdout "exit_code" of
                    Just s -> read (filter isDigit s) :: Int
                    _ -> 1
              return $ Right (success, stdoutVal, stderrVal, exitCodeVal)
            _ -> waitJobLoop jid (pollCount + 1) maxPolls

-- | Cancel a running job
cancelJob :: String -> IO Bool
cancelJob jobId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlDelete apiKey (apiBase ++ "/jobs/" ++ jobId)
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | List all active jobs
listJobs :: IO String
listJobs = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlGet apiKey (apiBase ++ "/jobs")
  return stdout

-- | List all sessions
sessionList :: IO String
sessionList = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlGet apiKey (apiBase ++ "/sessions")
  return stdout

-- | Get session details
sessionGet :: String -> IO String
sessionGet sessionId = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlGet apiKey (apiBase ++ "/sessions/" ++ sessionId)
  return stdout

-- | Create a new session
sessionCreate :: Maybe String -> Maybe String -> IO (Maybe String)
sessionCreate = sessionCreateWithOs Nothing

-- | Create a new session on a target OS
sessionCreateWithOs :: Maybe String -> Maybe String -> Maybe String -> IO (Maybe String)
sessionCreateWithOs targetOs shell network = do
  osJson <- osJsonFor targetOs
  apiKey <- getApiKey
  let shellVal = maybe "bash" id shell
  let networkJson = maybe "" (\n -> ",\"network\":\"" ++ n ++ "\"") network
  let json = "{\"shell\":\"" ++ shellVal ++ "\"" ++ networkJson ++ osJson ++ "}"
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/sessions") json
  return $ extractJsonString stdout "id"

-- | Destroy a session
sessionDestroy :: String -> IO Bool
sessionDestroy sessionId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlDelete apiKey (apiBase ++ "/sessions/" ++ sessionId)
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Freeze a session
sessionFreeze :: String -> IO Bool
sessionFreeze sessionId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/sessions/" ++ sessionId ++ "/freeze") "{}"
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Unfreeze a session
sessionUnfreeze :: String -> IO Bool
sessionUnfreeze sessionId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/sessions/" ++ sessionId ++ "/unfreeze") "{}"
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Boost session resources
sessionBoost :: String -> Int -> IO Bool
sessionBoost sessionId vcpu = do
  apiKey <- getApiKey
  let json = "{\"vcpu\":" ++ show vcpu ++ "}"
  (exitCode, stdout, _) <- curlPatch apiKey (apiBase ++ "/sessions/" ++ sessionId) json
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Unboost session
sessionUnboost :: String -> IO Bool
sessionUnboost sessionId = sessionBoost sessionId 1

-- | Execute a command in a session
sessionExecute :: String -> String -> IO String
sessionExecute sessionId command = do
  apiKey <- getApiKey
  let json = "{\"command\":\"" ++ escapeJSON command ++ "\"}"
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/sessions/" ++ sessionId ++ "/execute") json
  return stdout

-- | List all services
serviceList :: IO String
serviceList = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlGet apiKey (apiBase ++ "/services")
  return stdout

-- | Get service details
serviceGet :: String -> IO String
serviceGet serviceId = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlGet apiKey (apiBase ++ "/services/" ++ serviceId)
  return stdout

-- | Create a new service
serviceCreate :: String -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe String)
serviceCreate = serviceCreateWithOs Nothing

-- | Create a new service on a target OS
serviceCreateWithOs :: Maybe String -> String -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe String)
serviceCreateWithOs targetOs name ports bootstrap network = do
  osJson <- osJsonFor targetOs
  apiKey <- getApiKey
  let portsJson = maybe "" (\p -> ",\"ports\":[" ++ p ++ "]") ports
  let bootstrapJson = maybe "" (\b -> ",\"bootstrap\":\"" ++ escapeJSON b ++ "\"") bootstrap
  let networkJson = maybe "" (\n -> ",\"network\":\"" ++ n ++ "\"") network
  let json = "{\"name\":\"" ++ escapeJSON name ++ "\"" ++ portsJson ++ bootstrapJson ++ networkJson ++ osJson ++ "}"
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/services") json
  return $ extractJsonString stdout "id"

-- | Destroy a service
serviceDestroy :: String -> IO Bool
serviceDestroy serviceId = do
  result <- curlDeleteWithSudo "" (apiBase ++ "/services/" ++ serviceId)
  case result of
    SudoSuccess _ -> return True
    _ -> return False

-- | Freeze a service
serviceFreeze :: String -> IO Bool
serviceFreeze serviceId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/services/" ++ serviceId ++ "/freeze") "{}"
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Unfreeze a service
serviceUnfreeze :: String -> IO Bool
serviceUnfreeze serviceId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/services/" ++ serviceId ++ "/unfreeze") "{}"
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Lock a service
serviceLock :: String -> IO Bool
serviceLock serviceId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/services/" ++ serviceId ++ "/lock") "{}"
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Unlock a service
serviceUnlock :: String -> IO Bool
serviceUnlock serviceId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/services/" ++ serviceId ++ "/unlock") "{}"
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Set unfreeze-on-demand for a service
serviceSetUnfreezeOnDemand :: String -> Bool -> IO Bool
serviceSetUnfreezeOnDemand serviceId enabled = do
  apiKey <- getApiKey
  let enabledStr = if enabled then "true" else "false"
  let json = "{\"unfreeze_on_demand\":" ++ enabledStr ++ "}"
  (exitCode, stdout, _) <- curlPatch apiKey (apiBase ++ "/services/" ++ serviceId) json
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Redeploy a service
serviceRedeploy :: String -> Maybe String -> IO Bool
serviceRedeploy serviceId bootstrap = do
  apiKey <- getApiKey
  let bootstrapJson = maybe "" (\b -> "\"bootstrap\":\"" ++ escapeJSON b ++ "\"") bootstrap
  let json = "{" ++ bootstrapJson ++ "}"
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/services/" ++ serviceId ++ "/redeploy") json
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Get service logs
serviceLogs :: String -> Bool -> IO String
serviceLogs serviceId allLogs = do
  apiKey <- getApiKey
  let endpoint = if allLogs
        then "/services/" ++ serviceId ++ "/logs?all=true"
        else "/services/" ++ serviceId ++ "/logs"
  (_, stdout, _) <- curlGet apiKey (apiBase ++ endpoint)
  return stdout

-- | Execute a command in a service
serviceExecute :: String -> String -> IO String
serviceExecute serviceId command = do
  apiKey <- getApiKey
  let json = "{\"command\":\"" ++ escapeJSON command ++ "\"}"
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/services/" ++ serviceId ++ "/execute") json
  return stdout

-- | Resize a service
serviceResize :: String -> Int -> IO Bool
serviceResize serviceId vcpu = do
  apiKey <- getApiKey
  let json = "{\"vcpu\":" ++ show vcpu ++ "}"
  (exitCode, stdout, _) <- curlPatch apiKey (apiBase ++ "/services/" ++ serviceId) json
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | List all snapshots
snapshotList :: IO String
snapshotList = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlGet apiKey (apiBase ++ "/snapshots")
  return stdout

-- | Get snapshot details
snapshotGet :: String -> IO String
snapshotGet snapshotId = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlGet apiKey (apiBase ++ "/snapshots/" ++ snapshotId)
  return stdout

-- | Create a snapshot of a session
snapshotSession :: String -> Maybe String -> Bool -> IO (Maybe String)
snapshotSession sessionId name hot = do
  apiKey <- getApiKey
  let nameJson = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\",") name
  let hotJson = if hot then "\"hot\":true" else "\"hot\":false"
  let json = "{" ++ nameJson ++ hotJson ++ "}"
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/sessions/" ++ sessionId ++ "/snapshot") json
  return $ extractJsonString stdout "id"

-- | Create a snapshot of a service
snapshotService :: String -> Maybe String -> Bool -> IO (Maybe String)
snapshotService serviceId name hot = do
  apiKey <- getApiKey
  let nameJson = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\",") name
  let hotJson = if hot then "\"hot\":true" else "\"hot\":false"
  let json = "{" ++ nameJson ++ hotJson ++ "}"
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/services/" ++ serviceId ++ "/snapshot") json
  return $ extractJsonString stdout "id"

-- | Restore from a snapshot
snapshotRestore :: String -> IO (Maybe String)
snapshotRestore snapshotId = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/snapshots/" ++ snapshotId ++ "/restore") "{}"
  return $ extractJsonString stdout "id"

-- | Delete a snapshot
snapshotDelete :: String -> IO Bool
snapshotDelete snapshotId = do
  result <- curlDeleteWithSudo "" (apiBase ++ "/snapshots/" ++ snapshotId)
  case result of
    SudoSuccess _ -> return True
    _ -> return False

-- | Lock a snapshot
snapshotLock :: String -> IO Bool
snapshotLock snapshotId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/snapshots/" ++ snapshotId ++ "/lock") "{}"
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Unlock a snapshot
snapshotUnlock :: String -> IO Bool
snapshotUnlock snapshotId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/snapshots/" ++ snapshotId ++ "/unlock") "{}"
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Clone a snapshot to create a new session or service
snapshotClone :: String -> String -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe String)
snapshotClone snapshotId cloneType name ports shell = do
  apiKey <- getApiKey
  let typeJson = "\"type\":\"" ++ cloneType ++ "\""
  let nameJson = maybe "" (\n -> ",\"name\":\"" ++ escapeJSON n ++ "\"") name
  let portsJson = maybe "" (\p -> ",\"ports\":[" ++ p ++ "]") ports
  let shellJson = maybe "" (\s -> ",\"shell\":\"" ++ s ++ "\"") shell
  let json = "{" ++ typeJson ++ nameJson ++ portsJson ++ shellJson ++ "}"
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/snapshots/" ++ snapshotId ++ "/clone") json
  return $ extractJsonString stdout "id"

-- | List images
imageList :: Maybe String -> IO String
imageList filter' = do
  apiKey <- getApiKey
  let endpoint = maybe "/images" (\f -> "/images?filter=" ++ f) filter'
  (_, stdout, _) <- curlGet apiKey (apiBase ++ endpoint)
  return stdout

-- | Get image details
imageGet :: String -> IO String
imageGet imageId = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlGet apiKey (apiBase ++ "/images/" ++ imageId)
  return stdout

-- | Publish an image
imagePublish :: String -> String -> Maybe String -> Maybe String -> IO (Maybe String)
imagePublish sourceType sourceId name description = do
  apiKey <- getApiKey
  let nameJson = maybe "" (\n -> ",\"name\":\"" ++ escapeJSON n ++ "\"") name
  let descJson = maybe "" (\d -> ",\"description\":\"" ++ escapeJSON d ++ "\"") description
  let json = "{\"source_type\":\"" ++ sourceType ++ "\",\"source_id\":\"" ++ sourceId ++ "\"" ++ nameJson ++ descJson ++ "}"
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/images/publish") json
  return $ extractJsonString stdout "id"

-- | Delete an image
imageDelete :: String -> IO Bool
imageDelete imageId = do
  result <- curlDeleteWithSudo "" (apiBase ++ "/images/" ++ imageId)
  case result of
    SudoSuccess _ -> return True
    _ -> return False

-- | Lock an image
imageLock :: String -> IO Bool
imageLock imageId = do
  apiKey <- getApiKey
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/images/" ++ imageId ++ "/lock") "{}"
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Unlock an image
imageUnlock :: String -> IO Bool
imageUnlock imageId = do
  result <- curlPostWithSudo "" (apiBase ++ "/images/" ++ imageId ++ "/unlock") "{}"
  case result of
    SudoSuccess _ -> return True
    _ -> return False

-- | Set image visibility
imageSetVisibility :: String -> String -> IO Bool
imageSetVisibility imageId visibility = do
  apiKey <- getApiKey
  let json = "{\"visibility\":\"" ++ visibility ++ "\"}"
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/images/" ++ imageId ++ "/visibility") json
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Grant access to an image
imageGrantAccess :: String -> String -> IO Bool
imageGrantAccess imageId trustedApiKey = do
  apiKey <- getApiKey
  let json = "{\"api_key\":\"" ++ trustedApiKey ++ "\"}"
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/images/" ++ imageId ++ "/access/grant") json
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Revoke access to an image
imageRevokeAccess :: String -> String -> IO Bool
imageRevokeAccess imageId trustedApiKey = do
  apiKey <- getApiKey
  let json = "{\"api_key\":\"" ++ trustedApiKey ++ "\"}"
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/images/" ++ imageId ++ "/access/revoke") json
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | List trusted API keys for an image
imageListTrusted :: String -> IO String
imageListTrusted imageId = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlGet apiKey (apiBase ++ "/images/" ++ imageId ++ "/access")
  return stdout

-- | Transfer image ownership
imageTransfer :: String -> String -> IO Bool
imageTransfer imageId toApiKey = do
  apiKey <- getApiKey
  let json = "{\"to_api_key\":\"" ++ toApiKey ++ "\"}"
  (exitCode, stdout, _) <- curlPost apiKey (apiBase ++ "/images/" ++ imageId ++ "/transfer") json
  return $ exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') stdout)

-- | Spawn a service from an image
imageSpawn :: String -> Maybe String -> Maybe String -> Maybe String -> Maybe String -> IO (Maybe String)
imageSpawn imageId name ports bootstrap network = do
  apiKey <- getApiKey
  let nameJson = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\"") name
  let portsJson = maybe "" (\p -> (if null nameJson then "" else ",") ++ "\"ports\":[" ++ p ++ "]") ports
  let bootstrapJson = maybe "" (\b -> ",\"bootstrap\":\"" ++ escapeJSON b ++ "\"") bootstrap
  let networkJson = maybe "" (\n -> ",\"network\":\"" ++ n ++ "\"") network
  let json = "{" ++ nameJson ++ portsJson ++ bootstrapJson ++ networkJson ++ "}"
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/images/" ++ imageId ++ "/spawn") json
  return $ extractJsonString stdout "id"

-- | Clone an image
imageClone :: String -> Maybe String -> Maybe String -> IO (Maybe String)
imageClone imageId name description = do
  apiKey <- getApiKey
  let nameJson = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\"") name
  let descJson = maybe "" (\d -> (if null nameJson then "" else ",") ++ "\"description\":\"" ++ escapeJSON d ++ "\"") description
  let json = "{" ++ nameJson ++ descJson ++ "}"
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/images/" ++ imageId ++ "/clone") json
  return $ extractJsonString stdout "id"

-- | Validate API keys
validateKeys :: IO String
validateKeys = do
  apiKey <- getApiKey
  (_, stdout, _) <- curlPost apiKey (apiBase ++ "/keys/validate") "{}"
  return stdout

-- Helper for threadDelay (microseconds)
threadDelay :: Int -> IO ()
threadDelay us = do
  let ms = us `div` 1000
  _ <- readProcessWithExitCode "sleep" [show (fromIntegral ms / 1000.0 :: Double)] ""
  return ()

-- Strip --account N from argument list, set cliAccountIndex IORef
stripAccountArg :: [String] -> IO [String]
stripAccountArg [] = return []
stripAccountArg ("--account":n_str:rest) = do
  case reads n_str of
    [(n, "")] -> do
      writeIORef cliAccountIndex (Just n)
      stripAccountArg rest
    _ -> do
      hPutStrLn stderr "Error: --account requires an integer argument"
      exitFailure
stripAccountArg (arg:rest) = do
  rest' <- stripAccountArg rest
  return (arg : rest')

-- Strip global --os OS / --os=OS (before our subcommand or source file),
-- validate it and set cliOs IORef. Exits before any request.
stripOsArg :: [String] -> IO [String]
stripOsArg [] = return []
stripOsArg ["--os"] = osMissing
stripOsArg ("--os":v:rest) = setOs v >> stripOsArg rest
stripOsArg (arg:rest)
  | "--os=" `isPrefixOf` arg = setOs (drop 5 arg) >> stripOsArg rest
  | arg `elem` ["session", "service", "key", "snapshot", "image", "languages"] = return (arg : rest)
  | otherwise = do
      rest' <- stripOsArg rest
      return (arg : rest')

setOs :: String -> IO ()
setOs v = case normalizeOs v of
  Right o -> writeIORef cliOs (Just o)
  Left _ -> do
    hPutStrLn stderr $ "Error: invalid --os '" ++ v ++ "' (expected linux or freebsd)"
    exitFailure

osMissing :: IO a
osMissing = do
  hPutStrLn stderr "Error: --os requires a value: linux or freebsd"
  exitFailure

-- Main
main :: IO ()
main = do
  rawArgs <- getArgs
  osArgs <- stripOsArg rawArgs
  args <- stripAccountArg osArgs
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
  putStrLn "Global options:"
  putStrLn "  --account N     Use accounts.csv row N (bypasses env vars)"
  putStrLn "  --os OS         Target OS: linux (default) or freebsd (execute, session, service)"
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
  putStrLn "  --unfreeze-on-demand ID true|false  Enable/disable auto-unfreeze on HTTP request"
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
  putStrLn "  --rotate        Rotate this key's secret (sudo OTP), update accounts.csv"
  putStrLn "  --otp CODE      Finish a rotation started without a terminal"
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

  osJSON <- cliOsJson
  let json = "{\"language\":\"" ++ lang ++ "\",\"code\":\"" ++ escapeJSON code ++ "\""
             ++ envJSON ++ filesJSON ++ artifactsJSON ++ networkJSON ++ vcpuJSON ++ osJSON ++ "}"

  -- Call API
  (exitCode, stdout, stderr) <- curlPost apiKey (apiBase ++ "/execute") json

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
      (_, stdout, _) <- curlGet apiKey (apiBase ++ "/sessions")
      putStrLn stdout
    SessionKill sid -> do
      (_, stdout, _) <- curlDelete apiKey ((apiBase ++ "/sessions/") ++ sid)
      putStrLn $ green ++ "Session terminated: " ++ sid ++ reset
    SessionSnapshot sid -> do
      let nameJSON = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\",") (sessSnapshotName opts)
      let hotJSON = if sessHot opts then "\"hot\":true" else "\"hot\":false"
      let json = "{" ++ nameJSON ++ hotJSON ++ "}"
      hPutStrLn stderr $ "Creating snapshot of session " ++ sid ++ "..."
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/sessions/") ++ sid ++ "/snapshot") json
      putStrLn $ green ++ "Snapshot created" ++ reset
      putStrLn stdout
    SessionRestore snapshotId -> do
      -- --restore takes snapshot ID directly, calls /snapshots/:id/restore
      hPutStrLn stderr $ "Restoring from snapshot " ++ snapshotId ++ "..."
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/snapshots/") ++ snapshotId ++ "/restore") "{}"
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
      osJSON <- cliOsJson
      let json = "{\"shell\":\"" ++ shell ++ "\"" ++ networkJSON ++ vcpuJSON ++ filesJSON ++ osJSON ++ "}"
      (_, stdout, _) <- curlPost apiKey (apiBase ++ "/sessions") json
      putStrLn $ yellow ++ "Session created (WebSocket required for interactivity)" ++ reset
      putStrLn stdout

-- Service command
serviceCommand :: ServiceOpts -> IO ()
serviceCommand opts = do
  apiKey <- getApiKey
  case svcAction opts of
    ServiceList -> do
      (_, stdout, _) <- curlGet apiKey (apiBase ++ "/services")
      putStrLn stdout
    ServiceInfo sid -> do
      (_, stdout, _) <- curlGet apiKey ((apiBase ++ "/services/") ++ sid)
      putStrLn stdout
    ServiceLogs sid -> do
      (_, stdout, _) <- curlGet apiKey ((apiBase ++ "/services/") ++ sid ++ "/logs")
      putStrLn stdout
    ServiceSleep sid -> do
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/services/") ++ sid ++ "/freeze") "{}"
      putStrLn $ green ++ "Service frozen: " ++ sid ++ reset
    ServiceWake sid -> do
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/services/") ++ sid ++ "/unfreeze") "{}"
      putStrLn $ green ++ "Service unfreezing: " ++ sid ++ reset
    ServiceDestroy sid -> do
      result <- curlDeleteWithSudo apiKey ((apiBase ++ "/services/") ++ sid)
      case result of
        SudoSuccess _ -> putStrLn $ green ++ "Service destroyed: " ++ sid ++ reset
        SudoCancelled -> exitFailure
        SudoError msg -> do
          hPutStrLn stderr $ red ++ "Error: " ++ msg ++ reset
          exitFailure
    ServiceResize sid -> do
      case svcVcpu opts of
        Nothing -> do
          hPutStrLn stderr $ red ++ "Error: --resize requires -v N (1-8)" ++ reset
          exitFailure
        Just vcpu -> do
          let json = "{\"vcpu\":" ++ show vcpu ++ "}"
          let ram = vcpu * 2
          (_, stdout, _) <- curlPatch apiKey ((apiBase ++ "/services/") ++ sid) json
          putStrLn $ green ++ "Service resized to " ++ show vcpu ++ " vCPU, " ++ show ram ++ " GB RAM" ++ reset
    ServiceExecute sid cmd -> do
      let json = "{\"command\":\"" ++ escapeJSON cmd ++ "\"}"
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/services/") ++ sid ++ "/execute") json
      unless (null stdout) $ putStr $ blue ++ stdout ++ reset
    ServiceDumpBootstrap sid maybeFile -> do
      hPutStrLn stderr $ "Fetching bootstrap script from " ++ sid ++ "..."
      let json = "{\"command\":\"cat /tmp/bootstrap.sh\"}"
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/services/") ++ sid ++ "/execute") json
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
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/services/") ++ sid ++ "/snapshot") json
      putStrLn $ green ++ "Snapshot created" ++ reset
      putStrLn stdout
    ServiceRestore snapshotId -> do
      -- --restore takes snapshot ID directly, calls /snapshots/:id/restore
      hPutStrLn stderr $ "Restoring from snapshot " ++ snapshotId ++ "..."
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/snapshots/") ++ snapshotId ++ "/restore") "{}"
      putStrLn $ green ++ "Service restored from snapshot" ++ reset
      putStrLn stdout
    ServiceSetUnfreezeOnDemand sid enabled -> do
      let json = "{\"unfreeze_on_demand\":" ++ (if enabled then "true" else "false") ++ "}"
      (_, stdout, _) <- curlPatch apiKey ((apiBase ++ "/services/") ++ sid) json
      putStrLn $ green ++ "Service unfreeze_on_demand set to " ++ (if enabled then "true" else "false") ++ ": " ++ sid ++ reset
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
          osJSON <- cliOsJson
          let json = "{\"name\":\"" ++ name ++ "\"" ++ portsJSON ++ typeJSON ++ bootstrapJSON ++ bootstrapContentJSON ++ networkJSON ++ vcpuJSON ++ filesJSON ++ osJSON ++ "}"
          (_, stdout, _) <- curlPost apiKey (apiBase ++ "/services") json
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
  let path = drop (length apiBase) url
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
  let path = drop (length apiBase) url
  authHeaders <- buildAuthHeaders publicKey secretKey "GET" path ""
  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", url ] ++ authHeaders) ""
  -- Check for clock drift error
  checkClockDriftError stdout
  return (exitCode, stdout, stderr)

curlDelete :: String -> String -> IO (ExitCode, String, String)
curlDelete apiKey url = do
  (publicKey, secretKey) <- getApiKeys
  let path = drop (length apiBase) url
  authHeaders <- buildAuthHeaders publicKey secretKey "DELETE" path ""
  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", "-X", "DELETE", url ] ++ authHeaders) ""
  -- Check for clock drift error
  checkClockDriftError stdout
  return (exitCode, stdout, stderr)

curlPatch :: String -> String -> String -> IO (ExitCode, String, String)
curlPatch apiKey url body = do
  (publicKey, secretKey) <- getApiKeys
  let path = drop (length apiBase) url
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
  let path = drop (length apiBase) url
  authHeaders <- buildAuthHeaders publicKey secretKey "PUT" path body
  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", "-X", "PUT"
     , url
     , "-H", "Content-Type: text/plain"
     ] ++ authHeaders ++ ["-d", body]) ""
  checkClockDriftError stdout
  return (exitCode, stdout, stderr)

-- Result type for sudo challenge operations
data SudoResult = SudoSuccess String | SudoError String | SudoCancelled

-- Handle 428 sudo OTP challenge - prompts user for OTP and retries the request
handleSudoChallenge :: String -> String -> String -> String -> IO SudoResult
handleSudoChallenge response method endpoint body = do
  let challengeId = extractJsonString response "challenge_id"

  hPutStrLn stderr $ yellow ++ "Confirmation required. Check your email for a one-time code." ++ reset
  hPutStr stderr "Enter OTP: "
  hFlush stderr

  otpRaw <- getLine
  let otp = filter (/= '\n') $ filter (/= '\r') otpRaw

  if null otp
    then do
      hPutStrLn stderr $ red ++ "Error: Operation cancelled" ++ reset
      return SudoCancelled
    else do
      -- Retry the request with sudo headers
      (publicKey, secretKey) <- getApiKeys
      authHeaders <- buildAuthHeaders publicKey secretKey method endpoint body

      -- Build sudo headers
      let sudoHeaders = ["-H", "X-Sudo-OTP: " ++ otp] ++
                        case challengeId of
                          Just cid -> ["-H", "X-Sudo-Challenge: " ++ cid]
                          Nothing -> []

      let baseArgs = case method of
            "DELETE" -> ["-s", "-X", "DELETE", apiBase ++ endpoint]
            "POST" -> ["-s", "-X", "POST", apiBase ++ endpoint, "-H", "Content-Type: application/json", "-d", body]
            _ -> ["-s", apiBase ++ endpoint]

      (exitCode, retryStdout, _) <- readProcessWithExitCode "curl"
        (baseArgs ++ authHeaders ++ sudoHeaders) ""

      if exitCode == ExitSuccess && not ("\"error\"" `isPrefixOf` dropWhile (/= '"') retryStdout)
        then return $ SudoSuccess retryStdout
        else return $ SudoError retryStdout

-- Curl DELETE with 428 handling
curlDeleteWithSudo :: String -> String -> IO SudoResult
curlDeleteWithSudo apiKey url = do
  (publicKey, secretKey) <- getApiKeys
  let path = drop (length apiBase) url
  authHeaders <- buildAuthHeaders publicKey secretKey "DELETE" path ""

  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", "-X", "DELETE", "-w", "\n%{http_code}", url ] ++ authHeaders) ""

  -- Split response and status code
  let allLines = lines stdout
  let (bodyLines, statusLines) = splitAt (length allLines - 1) allLines
  let body = intercalate "\n" bodyLines
  let httpCode = case statusLines of
        [s] -> read (filter (`elem` "0123456789") s) :: Int
        _ -> 200

  checkClockDriftError body

  if httpCode == 428
    then handleSudoChallenge body "DELETE" path ""
    else return $ SudoSuccess body

-- Curl POST with 428 handling
curlPostWithSudo :: String -> String -> String -> IO SudoResult
curlPostWithSudo apiKey url body = do
  (publicKey, secretKey) <- getApiKeys
  let path = drop (length apiBase) url
  authHeaders <- buildAuthHeaders publicKey secretKey "POST" path body

  (exitCode, stdout, stderr) <- readProcessWithExitCode "curl"
    ([ "-s", "-X", "POST", "-w", "\n%{http_code}"
     , url
     , "-H", "Content-Type: application/json"
     ] ++ authHeaders ++ ["-d", body]) ""

  -- Split response and status code
  let allLines = lines stdout
  let (bodyLines, statusLines) = splitAt (length allLines - 1) allLines
  let bodyStr = intercalate "\n" bodyLines
  let httpCode = case statusLines of
        [s] -> read (filter (`elem` "0123456789") s) :: Int
        _ -> 200

  checkClockDriftError bodyStr

  if httpCode == 428
    then handleSudoChallenge bodyStr "POST" path body
    else return $ SudoSuccess bodyStr

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

-- Load credentials from a CSV file at a given account index
loadCredentialsFromCsv :: FilePath -> Int -> IO (Maybe (String, String))
loadCredentialsFromCsv csvPath accountIndex = do
  result <- (try (readFile csvPath) :: IO (Either IOError String))
  case result of
    Left _ -> return Nothing
    Right content -> do
      let ls = filter (\l -> not (null l) && head l /= '#') $
                 map trim $
                 lines content
          accounts = [ (pk', sk')
                     | l <- ls
                     , let (pk, rest) = break (== ',') l
                     , not (null rest)
                     , let pk' = trim pk
                           sk' = trim (takeWhile (/= ',') (drop 1 rest))
                     , length pk' > 8 && length sk' > 8
                     ]
      if accountIndex < length accounts then
        return $ Just (accounts !! accountIndex)
      else
        return Nothing
  where
    trim = reverse . dropWhile (`elem` " \t\r") . reverse . dropWhile (`elem` " \t\r")

-- Get API keys with correct priority:
--   1. --account N (cliAccountIndex IORef) -> accounts.csv row N
--   2. UNSANDBOX_PUBLIC_KEY / UNSANDBOX_SECRET_KEY env vars
--   3. ~/.unsandbox/accounts.csv row 0 (or UNSANDBOX_ACCOUNT env)
--   4. ./accounts.csv row 0
getApiKeys :: IO (String, Maybe String)
getApiKeys = do
  home <- maybe "." id <$> lookupEnv "HOME"
  let homeCsv = home ++ "/.unsandbox/accounts.csv"
  -- Priority 1: --account N
  mIdx <- readIORef cliAccountIndex
  case mIdx of
    Just idx -> do
      creds <- loadCredentialsFromCsv homeCsv idx
      case creds of
        Just (pk, sk) -> return (pk, Just sk)
        Nothing -> do
          creds2 <- loadCredentialsFromCsv "accounts.csv" idx
          case creds2 of
            Just (pk, sk) -> return (pk, Just sk)
            Nothing -> do
              hPutStrLn stderr $ "Error: No credentials found for account index " ++ show idx ++ " in accounts.csv"
              exitFailure
    Nothing -> do
      -- Priority 2: environment variables
      publicKey <- lookupEnv "UNSANDBOX_PUBLIC_KEY"
      secretKey <- lookupEnv "UNSANDBOX_SECRET_KEY"
      apiKey <- lookupEnv "UNSANDBOX_API_KEY"
      case (publicKey, secretKey, apiKey) of
        (Just pk, Just sk, _) -> return (pk, Just sk)
        (_, _, Just ak) -> return (ak, Nothing)
        _ -> do
          -- Priority 3: ~/.unsandbox/accounts.csv (or UNSANDBOX_ACCOUNT index)
          defaultIndexStr <- lookupEnv "UNSANDBOX_ACCOUNT"
          let defaultIndex = maybe 0 (\s -> case reads s of [(n,"")] -> n; _ -> 0) defaultIndexStr
          creds <- loadCredentialsFromCsv homeCsv defaultIndex
          case creds of
            Just (pk, sk) -> return (pk, Just sk)
            Nothing -> do
              -- Priority 4: ./accounts.csv
              creds2 <- loadCredentialsFromCsv "accounts.csv" defaultIndex
              case creds2 of
                Just (pk, sk) -> return (pk, Just sk)
                Nothing -> do
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
      mac = hmac secretBS messageBS :: HMAC SHA256
  in show (hmacGetDigest mac)

makeSignature :: String -> String -> String -> String -> String -> String
makeSignature secretKey timestamp method path body =
  -- Server verifies against the request path without its query string (conn.request_path)
  let signPath = takeWhile (/= '?') path
      message = timestamp ++ ":" ++ method ++ ":" ++ signPath ++ ":" ++ body
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
  case jsonFieldRaw resp "exit_code" of
    Just ('-':v) | not (null (takeWhile isDigit v)) -> negate (read (takeWhile isDigit v))
    Just v | not (null (takeWhile isDigit v)) -> read (takeWhile isDigit v)
    _ -> 0

-- Text following "field": (whitespace skipped) in a JSON string, if present
jsonFieldRaw :: String -> String -> Maybe String
jsonFieldRaw json field = go json
  where
    needle = "\"" ++ field ++ "\""
    go [] = Nothing
    go s@(_:xs)
      | needle `isPrefixOf` s =
          case dropWhile (`elem` " \t\r\n") (drop (length needle) s) of
            (':':v) -> Just (dropWhile (`elem` " \t\r\n") v)
            _ -> go xs
      | otherwise = go xs

-- Extract JSON string field (simple parser for basic cases)
extractJsonString :: String -> String -> Maybe String
extractJsonString json field =
  case jsonFieldRaw json field of
    Just ('"':value) -> Just (takeWhile (/= '"') value)
    Just v | not (null v) && (isDigit (head v) || head v == '-') -> Just (takeWhile (`notElem` ",} \t\r\n") v)
    _ -> Nothing

-- Snapshot command
snapshotCommand :: SnapshotOpts -> IO ()
snapshotCommand opts = do
  apiKey <- getApiKey
  case snapAction opts of
    SnapshotList -> do
      (_, stdout, _) <- curlGet apiKey (apiBase ++ "/snapshots")
      putStrLn stdout
    SnapshotInfo sid -> do
      (_, stdout, _) <- curlGet apiKey ((apiBase ++ "/snapshots/") ++ sid)
      putStrLn stdout
    SnapshotDelete sid -> do
      result <- curlDeleteWithSudo apiKey ((apiBase ++ "/snapshots/") ++ sid)
      case result of
        SudoSuccess _ -> putStrLn $ green ++ "Snapshot deleted: " ++ sid ++ reset
        SudoCancelled -> exitFailure
        SudoError msg -> do
          hPutStrLn stderr $ red ++ "Error: " ++ msg ++ reset
          exitFailure
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
          (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/snapshots/") ++ sid ++ "/clone") json
          putStrLn $ green ++ "Snapshot cloned" ++ reset
          putStrLn stdout

-- Image command
imageCommand :: ImageOpts -> IO ()
imageCommand opts = do
  apiKey <- getApiKey
  case imgAction opts of
    ImageList -> do
      (_, stdout, _) <- curlGet apiKey (apiBase ++ "/images")
      putStrLn stdout
    ImageInfo iid -> do
      (_, stdout, _) <- curlGet apiKey ((apiBase ++ "/images/") ++ iid)
      putStrLn stdout
    ImageDelete iid -> do
      result <- curlDeleteWithSudo apiKey ((apiBase ++ "/images/") ++ iid)
      case result of
        SudoSuccess _ -> putStrLn $ green ++ "Image deleted: " ++ iid ++ reset
        SudoCancelled -> exitFailure
        SudoError msg -> do
          hPutStrLn stderr $ red ++ "Error: " ++ msg ++ reset
          exitFailure
    ImageLock iid -> do
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/images/") ++ iid ++ "/lock") "{}"
      putStrLn $ green ++ "Image locked: " ++ iid ++ reset
    ImageUnlock iid -> do
      result <- curlPostWithSudo apiKey ((apiBase ++ "/images/") ++ iid ++ "/unlock") "{}"
      case result of
        SudoSuccess _ -> putStrLn $ green ++ "Image unlocked: " ++ iid ++ reset
        SudoCancelled -> exitFailure
        SudoError msg -> do
          hPutStrLn stderr $ red ++ "Error: " ++ msg ++ reset
          exitFailure
    ImagePublish sourceId -> do
      case imgSourceType opts of
        Nothing -> do
          hPutStrLn stderr $ red ++ "Error: --source-type required (service or snapshot)" ++ reset
          exitFailure
        Just sourceType -> do
          let nameJSON = maybe "" (\n -> ",\"name\":\"" ++ escapeJSON n ++ "\"") (imgName opts)
          let json = "{\"source_type\":\"" ++ sourceType ++ "\",\"source_id\":\"" ++ sourceId ++ "\"" ++ nameJSON ++ "}"
          (_, stdout, _) <- curlPost apiKey (apiBase ++ "/images/publish") json
          putStrLn $ green ++ "Image published" ++ reset
          putStrLn stdout
    ImageVisibility iid -> do
      case imgVisibilityMode opts of
        Nothing -> do
          hPutStrLn stderr $ red ++ "Error: --visibility requires MODE (private, unlisted, or public)" ++ reset
          exitFailure
        Just mode -> do
          let json = "{\"visibility\":\"" ++ mode ++ "\"}"
          (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/images/") ++ iid ++ "/visibility") json
          putStrLn $ green ++ "Image visibility set to " ++ mode ++ ": " ++ iid ++ reset
    ImageSpawn iid -> do
      let nameJSON = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\"") (imgName opts)
      let portsJSON = maybe "" (\p -> ",\"ports\":[" ++ p ++ "]") (imgPorts opts)
      let json = if null nameJSON then "{" ++ (if null portsJSON then "" else drop 1 portsJSON) ++ "}"
                 else "{" ++ nameJSON ++ portsJSON ++ "}"
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/images/") ++ iid ++ "/spawn") json
      putStrLn $ green ++ "Service spawned from image" ++ reset
      putStrLn stdout
    ImageClone iid -> do
      let nameJSON = maybe "" (\n -> "\"name\":\"" ++ escapeJSON n ++ "\"") (imgName opts)
      let json = "{" ++ nameJSON ++ "}"
      (_, stdout, _) <- curlPost apiKey ((apiBase ++ "/images/") ++ iid ++ "/clone") json
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
      return $ posixSecondsToUTCTime $ realToFrac $ System.Posix.Files.modificationTime status
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
      (_, stdout, _) <- curlGet apiKey (apiBase ++ "/languages")
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
keyCommand opts | keyRotate opts = rotateKeyCommand (keyOtp opts)
keyCommand opts = do
  apiKey <- getApiKey

  if keyExtend opts
    then extendKey apiKey
    else validateKey apiKey

-- POST /keys/validate, empty body, signed; headers reach curl on stdin (-K -)
keyValidatePost :: String -> Maybe String -> IO (Int, String)
keyValidatePost publicKey mSecret = do
  let path = "/keys/validate"
  authHeaders <- buildAuthHeaders publicKey mSecret "POST" path ""
  let pairs ("-H" : h : rest) = h : pairs rest
      pairs (_ : rest) = pairs rest
      pairs [] = []
      headers = ["Content-Type: application/json"] ++ pairs authHeaders
      config = unlines [ "header = " ++ curlConfigQuote h | h <- headers ]
  (ec, out, _) <- readProcessWithExitCode "curl"
    [ "-s", "-X", "POST", "-w", "\n%{http_code}", "-K", "-", apiBase ++ path ] config
  let allLines = lines out
      (bodyLines, statusLines) = splitAt (length allLines - 1) allLines
      code = case statusLines of
        [st] | not (null (filter isDigit st)) -> read (filter isDigit st)
        _ -> 0
  return (if ec /= ExitSuccess && code == 0 then 0 else code, intercalate "\n" bodyLines)

-- Validate API key and show status (CLI_SPEC.md "un key output")
validateKey :: String -> IO ()
validateKey _ = do
  (publicKey, mSecret) <- getApiKeys
  (code, resp) <- keyValidatePost publicKey mSecret
  let strField f = case jsonFieldRaw resp f of
        Just ('"':v) -> Just (takeWhile (/= '"') v)
        _ -> Nothing
      intField f = case jsonFieldRaw resp f of
        Just v@(c:_) | isDigit c || c == '-' ->
          let n = takeWhile (\x -> isDigit x || x == '-') v in Just (read n :: Integer)
        _ -> Nothing
  (state, reason) <- case code of
    200 -> do
      let st = case strField "status" of
            Just x -> x
            Nothing -> case jsonFieldRaw resp "valid" of
              Just v | "true" `isPrefixOf` v -> "active"
              _ -> "invalid"
      return (st, "status " ++ st)
    401 -> do
      let err = maybe "unauthorized" id (strField "error")
          st | err `elem` [":expired", "expired"] = "expired"
             | err `elem` [":suspended", "suspended"] = "suspended"
             | otherwise = "invalid"
      return (st, err)
    0 -> do
      hPutStrLn stderr $ red ++ "Error: request to " ++ apiBase ++ "/keys/validate failed" ++ reset
      exitWith (ExitFailure 1)
    _ -> do
      let msg = maybe (maybe "" id (strField "message")) id (strField "error")
      hPutStrLn stderr $ red ++ "Error: HTTP " ++ show code ++ " " ++ msg ++ reset
      exitWith (ExitFailure 4)
  case state of
    "active" -> do
      now <- getPOSIXTime
      let nowS = floor now :: Integer
          (expires, remaining) = case intField "valid_through" of
            Nothing -> ("N/A", "N/A")
            Just vt ->
              let secs = vt `div` 1000
                  exp' = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ"
                           (posixSecondsToUTCTime (fromIntegral secs))
                  left = secs - nowS
                  rem' = if left <= 0 then "expired"
                         else show (left `div` 86400) ++ "d " ++ show ((left `mod` 86400) `div` 3600)
                              ++ "h " ++ show ((left `mod` 3600) `div` 60) ++ "m"
              in (exp', rem')
      putStrLn $ green ++ "Valid" ++ reset
      putStrLn $ "Public Key: " ++ publicKey
      putStrLn $ "Tier: " ++ maybe "N/A" show (intField "tier")
      putStrLn $ "Status: " ++ maybe "N/A" id (strField "status")
      putStrLn $ "Expires: " ++ expires
      putStrLn $ "Time Remaining: " ++ remaining
    "expired" -> do
      putStrLn $ red ++ "Expired" ++ reset
      putStrLn $ "Public Key: " ++ publicKey
      putStrLn $ yellow ++ "To renew: Visit " ++ portalBase ++ "/keys/extend" ++ reset
      exitWith (ExitFailure 3)
    "suspended" -> do
      putStrLn $ red ++ "Suspended" ++ reset
      putStrLn $ "Public Key: " ++ publicKey
      exitWith (ExitFailure 3)
    _ -> do
      putStrLn $ red ++ "Invalid" ++ reset
      putStrLn $ "Public Key: " ++ publicKey
      putStrLn $ "Reason: " ++ reason
      exitWith (ExitFailure 3)

-- Extend key (open browser to extend page)
extendKey :: String -> IO ()
extendKey _ = do
  (publicKey, _) <- getApiKeys
  let extendUrl = portalBase ++ "/keys/extend?pk=" ++ publicKey
  putStrLn "Opening extension page in browser..."
  putStrLn $ "If browser doesn't open, visit: " ++ extendUrl
  _ <- readProcessWithExitCode "sh"
    ["-c", "xdg-open '" ++ extendUrl ++ "' 2>/dev/null || sensible-browser '" ++ extendUrl ++ "' 2>/dev/null || true"]
    ""
  return ()

-- ============================================================================
-- un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md)
-- ============================================================================

-- | The accounts.csv our credentials came from, or Nothing (env vars).
-- Mirrors getApiKeys' priority order.
credentialsCsvPath :: IO (Maybe FilePath)
credentialsCsvPath = do
  home <- maybe "." id <$> lookupEnv "HOME"
  let homeCsv = home ++ "/.unsandbox/accounts.csv"
  mIdx <- readIORef cliAccountIndex
  publicKey <- lookupEnv "UNSANDBOX_PUBLIC_KEY"
  secretKey <- lookupEnv "UNSANDBOX_SECRET_KEY"
  apiKey <- lookupEnv "UNSANDBOX_API_KEY"
  defaultIndexStr <- lookupEnv "UNSANDBOX_ACCOUNT"
  let defaultIndex = maybe 0 (\s -> case reads s of [(n,"")] -> n; _ -> 0) defaultIndexStr
      fromCsv idx = do
        c1 <- loadCredentialsFromCsv homeCsv idx
        case c1 of
          Just _ -> return (Just homeCsv)
          Nothing -> do
            c2 <- loadCredentialsFromCsv "accounts.csv" idx
            return $ maybe Nothing (const (Just "accounts.csv")) c2
  case (mIdx, publicKey, secretKey, apiKey) of
    (Just idx, _, _, _) -> fromCsv idx
    (Nothing, Just _, Just _, _) -> return Nothing
    (Nothing, _, _, Just _) -> return Nothing
    _ -> fromCsv defaultIndex

-- | Split on a separator, keeping empty pieces (so join restores the input exactly)
splitOnChar :: Char -> String -> [String]
splitOnChar c str = case break (== c) str of
  (a, []) -> [a]
  (a, _:rest) -> a : splitOnChar c rest

-- | Write a file created 0600 (umask 077 while creating, chmod after)
writePrivateFile :: FilePath -> String -> IO ()
writePrivateFile path content = do
  old <- System.Posix.Files.setFileCreationMask 0o077
  writeFile path content
  _ <- System.Posix.Files.setFileCreationMask old
  System.Posix.Files.setFileMode path 0o600

-- | Replace the secret on the line whose first field equals publicKey. Every
-- other line, field, comment and the order stay as they are. Temp file + rename.
rewriteAccountsCsv :: FilePath -> String -> String -> IO Bool
rewriteAccountsCsv path publicKey newSecret = do
  content <- BSC.unpack <$> BS.readFile path
  let trim = reverse . dropWhile (`elem` " \t\r") . reverse . dropWhile (`elem` " \t\r")
      step (done, acc) line
        | done = (True, line : acc)
        | null (trim line) || "#" `isPrefixOf` trim line = (False, line : acc)
        | otherwise = case splitOnChar ',' line of
            (pk : _ : rest) | trim pk == publicKey ->
              (True, intercalate "," (pk : newSecret : rest) : acc)
            _ -> (False, line : acc)
      (changed, revLines) = foldl step (False, []) (splitOnChar '\n' content)
  if not changed
    then return False
    else do
      pid <- System.Posix.Process.getProcessID
      let tmp = takeDirectory path </> ("." ++ takeFileName path ++ "." ++ show pid ++ ".tmp")
      writePrivateFile tmp (intercalate "\n" (reverse revLines))
      System.Posix.Files.rename tmp path
      return True

-- | Escape a value for a curl config file string
curlConfigQuote :: String -> String
curlConfigQuote v = "\"" ++ concatMap esc v ++ "\""
  where
    esc '"' = "\\\""
    esc '\\' = "\\\\"
    esc c = [c]

-- | POST /keys/self/rotate-secret with a fresh signature. Headers reach curl
-- via a config on stdin, never on its command line. Returns (status, body).
rotatePost :: String -> String -> Maybe (String, String) -> IO (Int, String)
rotatePost publicKey secretKey sudo = do
  let path = "/keys/self/rotate-secret"
      body = "{}"
  authHeaders <- buildAuthHeaders publicKey (Just secretKey) "POST" path body
  let pairs ("-H" : h : rest) = h : pairs rest
      pairs (_ : rest) = pairs rest
      pairs [] = []
      sudoHeaders = case sudo of
        Just (otp, challenge) -> ["X-Sudo-OTP: " ++ otp, "X-Sudo-Challenge: " ++ challenge]
        Nothing -> []
      headers = ["Content-Type: application/json"] ++ pairs authHeaders ++ sudoHeaders
      config = unlines [ "header = " ++ curlConfigQuote h | h <- headers ]
  (_, out, _) <- readProcessWithExitCode "curl"
    [ "-s", "-X", "POST", "-w", "\n%{http_code}", "-K", "-", "--data-binary", body
    , apiBase ++ path ] config
  let allLines = lines out
      (bodyLines, statusLines) = splitAt (length allLines - 1) allLines
      code = case statusLines of
        [st] | not (null (filter isDigit st)) -> read (filter isDigit st)
        _ -> 0
  return (code, intercalate "\n" bodyLines)

rotateKeyCommand :: Maybe String -> IO ()
rotateKeyCommand mOtp = do
  (publicKey, mSecret) <- getApiKeys
  secretKey <- case mSecret of
    Just sk -> return sk
    Nothing -> do
      hPutStrLn stderr "Error: rotation needs UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY"
      exitWith (ExitFailure 3)
  home <- maybe "." id <$> lookupEnv "HOME"
  let unsandboxDir = home ++ "/.unsandbox"
      digest = show (hashWith SHA256 (BSC.pack publicKey))
      challengeFile = unsandboxDir ++ "/rotate-" ++ take 16 digest ++ ".challenge"
      strip = reverse . dropWhile (`elem` " \t\r\n") . reverse . dropWhile (`elem` " \t\r\n")
      removeChallenge = do
        _ <- try (System.Posix.Files.removeLink challengeFile) :: IO (Either IOError ())
        return ()
      failWith (code, body)
        | code == 403 && extractJsonString body "error" == Just "email_required" = do
            hPutStrLn stderr "Error: this key has no email; add one or use POST /keys/recover"
            exitWith (ExitFailure 3)
        | code == 401 = do
            hPutStrLn stderr "Error: authentication failed"
            exitWith (ExitFailure 3)
        | otherwise = do
            let msg = case (extractJsonString body "message", extractJsonString body "error") of
                  (Just m, _) -> m
                  (_, Just e) -> e
                  _ -> "HTTP " ++ show code
            hPutStrLn stderr $ "Error: " ++ msg
            exitWith (ExitFailure 4)

  result <- case mOtp of
    Just otp -> do
      saved <- try (BSC.unpack <$> BS.readFile challengeFile) :: IO (Either IOError String)
      case saved of
        Left _ -> do
          hPutStrLn stderr "Error: no rotation in progress; run: un key --rotate"
          exitWith (ExitFailure 2)
        Right challenge -> do
          r <- rotatePost publicKey secretKey (Just (strip otp, strip challenge))
          removeChallenge
          return (Just r)
    Nothing -> do
      r@(code, body) <- rotatePost publicKey secretKey Nothing
      if code /= 428
        then return (Just r)
        else do
          let challenge = maybe "" id (extractJsonString body "challenge_id")
          tty <- hIsTerminalDevice stdin
          if tty
            then do
              hPutStrLn stderr "One-time code emailed to the key owner."
              hPutStr stderr "Code: "
              hFlush stderr
              otp <- strip <$> getLine
              when (null otp) $ do
                hPutStrLn stderr "Error: cancelled"
                exitFailure
              Just <$> rotatePost publicKey secretKey (Just (otp, challenge))
            else do
              existed <- System.Posix.Files.fileExist unsandboxDir
              unless existed $ do
                createDirectoryIfMissing True unsandboxDir
                System.Posix.Files.setFileMode unsandboxDir 0o700
              writePrivateFile challengeFile challenge
              putStrLn "One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE"
              return Nothing

  case result of
    Nothing -> return ()
    Just (code, body) -> do
      when (code /= 200) $ failWith (code, body)
      newSecret <- case extractJsonString body "secret" of
        Just sk | not (null sk) -> return sk
        _ -> do
          hPutStrLn stderr "Error: API returned no secret"
          exitWith (ExitFailure 4)
      csvPath <- credentialsCsvPath
      updated <- case csvPath of
        Just p -> rewriteAccountsCsv p publicKey newSecret
        Nothing -> return False
      case csvPath of
        Just p | updated ->
          putStrLn $ "Secret rotated. Updated " ++ p ++ " (public key " ++ publicKey ++ ")."
        _ -> do
          putStrLn $ "New secret (shown once, store it now): " ++ newSecret
          putStrLn "Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret."
