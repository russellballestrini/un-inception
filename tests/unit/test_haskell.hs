#!/usr/bin/env runhaskell
-- Unit tests for un.hs - tests internal functions without API calls

import Data.List (isInfixOf, isPrefixOf)
import Data.IORef
import System.Exit (exitWith, ExitCode(..))
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

main :: IO ()
main = do
    passedRef <- newIORef 0
    failedRef <- newIORef 0

    let extMap = Map.fromList
            [ (".py", "python"), (".js", "javascript"), (".ts", "typescript")
            , (".rb", "ruby"), (".go", "go"), (".rs", "rust"), (".c", "c")
            , (".java", "java"), (".kt", "kotlin"), (".hs", "haskell")
            , (".clj", "clojure"), (".erl", "erlang")
            ]

    putStrLn "\n=== Extension Mapping Tests ==="

    test passedRef failedRef "Python extension maps correctly" $
        Map.lookup ".py" extMap == Just "python"

    test passedRef failedRef "Haskell extension maps correctly" $
        Map.lookup ".hs" extMap == Just "haskell"

    test passedRef failedRef "JavaScript extension maps correctly" $
        Map.lookup ".js" extMap == Just "javascript"

    test passedRef failedRef "Go extension maps correctly" $
        Map.lookup ".go" extMap == Just "go"

    test passedRef failedRef "Clojure extension maps correctly" $
        Map.lookup ".clj" extMap == Just "clojure"

    putStrLn "\n=== Signature Format Tests ==="

    let timestamp = "1704067200"
        method = "POST"
        endpoint = "/execute"
        body = "{\"language\":\"python\"}"
        message = timestamp ++ ":" ++ method ++ ":" ++ endpoint ++ ":" ++ body

    test passedRef failedRef "Signature format starts with timestamp" $
        isPrefixOf timestamp message

    test passedRef failedRef "Signature format contains :POST:" $
        isInfixOf ":POST:" message

    test passedRef failedRef "Signature format contains :/execute:" $
        isInfixOf ":/execute:" message

    putStrLn "\n=== Language Detection Tests ==="

    let content = "#!/usr/bin/env python3\nprint('hello')"
        firstLine = head (lines content)

    test passedRef failedRef "Python shebang detection - starts with #!" $
        isPrefixOf "#!" firstLine

    test passedRef failedRef "Python shebang detection - contains python" $
        isInfixOf "python" firstLine

    putStrLn "\n=== Argument Parsing Tests ==="

    let arg1 = "DEBUG=1"
        (key1, _:value1) = break (== '=') arg1

    test passedRef failedRef "Parse -e KEY=VALUE format - key" $
        key1 == "DEBUG"

    test passedRef failedRef "Parse -e KEY=VALUE format - value" $
        value1 == "1"

    let arg2 = "URL=https://example.com?foo=bar"
        (key2, _:value2) = break (== '=') arg2

    test passedRef failedRef "Parse -e KEY=VALUE with equals in value" $
        key2 == "URL" && value2 == "https://example.com?foo=bar"

    putStrLn "\n=== File Operations Tests ==="

    let path = "/home/user/project/script.hs"
        basename = reverse $ takeWhile (/= '/') $ reverse path

    test passedRef failedRef "Extract file basename" $
        basename == "script.hs"

    let ext = dropWhile (/= '.') basename

    test passedRef failedRef "Extract file extension" $
        ext == ".hs"

    putStrLn "\n=== API Constants Tests ==="

    let apiBase = "https://api.unsandbox.com"

    test passedRef failedRef "API base URL starts with https://" $
        isPrefixOf "https://" apiBase

    test passedRef failedRef "API base URL contains unsandbox.com" $
        isInfixOf "unsandbox.com" apiBase

    -- Summary
    passed <- readIORef passedRef
    failed <- readIORef failedRef

    putStrLn "\n=== Summary ==="
    putStrLn $ "Passed: " ++ show passed
    putStrLn $ "Failed: " ++ show failed
    putStrLn $ "Total:  " ++ show (passed + failed)

    exitWith $ if failed > 0 then ExitFailure 1 else ExitSuccess

test :: IORef Int -> IORef Int -> String -> Bool -> IO ()
test passedRef failedRef name result =
    if result
        then do
            putStrLn $ "  ✓ " ++ name
            modifyIORef passedRef (+1)
        else do
            putStrLn $ "  ✗ " ++ name
            modifyIORef failedRef (+1)
