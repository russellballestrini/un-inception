#!/usr/bin/env lua
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

-- un.lua - Unsandbox CLI Client (Lua Implementation)
--
-- Full-featured CLI matching un.c capabilities:
-- - Execute code with env vars, input files, artifacts
-- - Interactive sessions with shell/REPL support
-- - Persistent services with domains and ports
--
-- Usage:
--   un.lua [options] <source_file>
--   un.lua session [options]
--   un.lua service [options]
--
-- Requires: UNSANDBOX_API_KEY environment variable
-- Note: Uses curl for HTTP requests (requires curl to be installed)

local json = require("cjson")

local API_BASE = "https://api.unsandbox.com"
local PORTAL_BASE = "https://unsandbox.com"
local BLUE = "\27[34m"
local RED = "\27[31m"
local GREEN = "\27[32m"
local YELLOW = "\27[33m"
local RESET = "\27[0m"

local EXT_MAP = {
    [".py"] = "python", [".js"] = "javascript", [".ts"] = "typescript",
    [".rb"] = "ruby", [".php"] = "php", [".pl"] = "perl", [".lua"] = "lua",
    [".sh"] = "bash", [".go"] = "go", [".rs"] = "rust", [".c"] = "c",
    [".cpp"] = "cpp", [".cc"] = "cpp", [".cxx"] = "cpp",
    [".java"] = "java", [".kt"] = "kotlin", [".cs"] = "csharp", [".fs"] = "fsharp",
    [".hs"] = "haskell", [".ml"] = "ocaml", [".clj"] = "clojure", [".scm"] = "scheme",
    [".lisp"] = "commonlisp", [".erl"] = "erlang", [".ex"] = "elixir", [".exs"] = "elixir",
    [".jl"] = "julia", [".r"] = "r", [".R"] = "r", [".cr"] = "crystal",
    [".d"] = "d", [".nim"] = "nim", [".zig"] = "zig", [".v"] = "v",
    [".dart"] = "dart", [".groovy"] = "groovy", [".scala"] = "scala",
    [".f90"] = "fortran", [".f95"] = "fortran", [".cob"] = "cobol",
    [".pro"] = "prolog", [".forth"] = "forth", [".4th"] = "forth",
    [".tcl"] = "tcl", [".raku"] = "raku", [".m"] = "objc"
}

local function get_api_keys(args_key)
    local public_key = os.getenv("UNSANDBOX_PUBLIC_KEY")
    local secret_key = os.getenv("UNSANDBOX_SECRET_KEY")

    if not public_key or not secret_key then
        local old_key = args_key or os.getenv("UNSANDBOX_API_KEY")
        if old_key then
            public_key = old_key
            secret_key = old_key
        else
            io.stderr:write(RED .. "Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set" .. RESET .. "\n")
            io.stderr:write(RED .. "       (or legacy UNSANDBOX_API_KEY for backwards compatibility)" .. RESET .. "\n")
            os.exit(1)
        end
    end

    return {public_key = public_key, secret_key = secret_key}
end

local function detect_language(filename)
    local ext = filename:match("%.([^.]+)$")
    if ext then
        local lang = EXT_MAP["." .. ext:lower()]
        if lang then return lang end
    end

    local file = io.open(filename, "r")
    if file then
        local first_line = file:read("*line")
        file:close()
        if first_line and first_line:match("^#!") then
            if first_line:match("python") then return "python" end
            if first_line:match("node") then return "javascript" end
            if first_line:match("ruby") then return "ruby" end
            if first_line:match("perl") then return "perl" end
            if first_line:match("bash") or first_line:match("/sh") then return "bash" end
            if first_line:match("lua") then return "lua" end
            if first_line:match("php") then return "php" end
        end
    end

    io.stderr:write(RED .. "Error: Cannot detect language for " .. filename .. RESET .. "\n")
    os.exit(1)
end

local function shell_escape(str)
    return "'" .. str:gsub("'", "'\\''") .. "'"
end

local function api_request(endpoint, method, data, keys)
    method = method or "GET"
    local url = API_BASE .. endpoint
    local tmpfile = os.tmpname()

    -- Generate timestamp and signature
    local timestamp = tostring(os.time())
    local body = data and json.encode(data) or ""

    -- Parse URL to get path
    local path = endpoint

    -- Create HMAC signature using openssl command
    local message = timestamp .. ":" .. method .. ":" .. path .. ":" .. body
    local sig_tmpfile = os.tmpname()
    local msg_tmpfile = os.tmpname()

    -- Write message to temp file
    local f = io.open(msg_tmpfile, "w")
    f:write(message)
    f:close()

    -- Generate HMAC using openssl
    local hmac_cmd = "openssl dgst -sha256 -hmac " .. shell_escape(keys.secret_key) .. " -hex " .. shell_escape(msg_tmpfile) .. " | awk '{print $2}'"
    local sig_handle = io.popen(hmac_cmd)
    local signature = sig_handle:read("*a"):gsub("%s+$", "")
    sig_handle:close()
    os.remove(msg_tmpfile)

    local cmd = "curl -s -X " .. method .. " " .. shell_escape(url) ..
                " -H 'Authorization: Bearer " .. keys.public_key .. "'" ..
                " -H 'X-Timestamp: " .. timestamp .. "'" ..
                " -H 'X-Signature: " .. signature .. "'" ..
                " -H 'Content-Type: application/json'"

    local data_file
    if data then
        data_file = os.tmpname()
        local f = io.open(data_file, "w")
        f:write(body)
        f:close()
        cmd = cmd .. " -d @" .. shell_escape(data_file)
    end

    cmd = cmd .. " -w '\\n%{http_code}' -o " .. shell_escape(tmpfile)

    local handle = io.popen(cmd)
    local http_code = handle:read("*a"):match("(%d+)$")
    handle:close()

    local file = io.open(tmpfile, "r")
    local response = file:read("*all")
    file:close()
    os.remove(tmpfile)

    if data_file then
        os.remove(data_file)
    end

    if not http_code or tonumber(http_code) < 200 or tonumber(http_code) >= 300 then
        if http_code == "401" and response:lower():find("timestamp") then
            io.stderr:write(RED .. "Error: Request timestamp expired (must be within 5 minutes of server time)" .. RESET .. "\n")
            io.stderr:write(YELLOW .. "Your computer's clock may have drifted." .. RESET .. "\n")
            io.stderr:write(YELLOW .. "Check your system time and sync with NTP if needed:" .. RESET .. "\n")
            io.stderr:write("  Linux:   sudo ntpdate -s time.nist.gov\n")
            io.stderr:write("  macOS:   sudo sntp -sS time.apple.com\n")
            io.stderr:write("  Windows: w32tm /resync\n")
        else
            io.stderr:write(RED .. "Error: HTTP " .. (http_code or "000") .. " - " .. response .. RESET .. "\n")
        end
        os.exit(1)
    end

    return json.decode(response)
end

local function api_request_text(endpoint, method, body, keys)
    local url = API_BASE .. endpoint
    local tmpfile = os.tmpname()

    -- Generate timestamp and signature
    local timestamp = tostring(os.time())
    local path = endpoint

    -- Create HMAC signature using openssl command
    local message = timestamp .. ":" .. method .. ":" .. path .. ":" .. body
    local msg_tmpfile = os.tmpname()

    local f = io.open(msg_tmpfile, "w")
    f:write(message)
    f:close()

    local hmac_cmd = "openssl dgst -sha256 -hmac " .. shell_escape(keys.secret_key) .. " -hex " .. shell_escape(msg_tmpfile) .. " | awk '{print $2}'"
    local sig_handle = io.popen(hmac_cmd)
    local signature = sig_handle:read("*a"):gsub("%s+$", "")
    sig_handle:close()
    os.remove(msg_tmpfile)

    local data_file = os.tmpname()
    local df = io.open(data_file, "w")
    df:write(body)
    df:close()

    local cmd = "curl -s -X " .. method .. " " .. shell_escape(url) ..
                " -H 'Authorization: Bearer " .. keys.public_key .. "'" ..
                " -H 'X-Timestamp: " .. timestamp .. "'" ..
                " -H 'X-Signature: " .. signature .. "'" ..
                " -H 'Content-Type: text/plain'" ..
                " -d @" .. shell_escape(data_file) ..
                " -w '\\n%{http_code}' -o " .. shell_escape(tmpfile)

    local handle = io.popen(cmd)
    local http_code = handle:read("*a"):match("(%d+)$")
    handle:close()

    local file = io.open(tmpfile, "r")
    local response = file:read("*all")
    file:close()
    os.remove(tmpfile)
    os.remove(data_file)

    if not http_code or tonumber(http_code) < 200 or tonumber(http_code) >= 300 then
        return { error = "HTTP " .. (http_code or "000") .. " - " .. response }
    end

    return json.decode(response)
end

-- ============================================================================
-- Environment Secrets Vault Functions
-- ============================================================================

local MAX_ENV_CONTENT_SIZE = 64 * 1024 -- 64KB max

local function service_env_status(service_id, keys)
    local result = api_request("/services/" .. service_id .. "/env", "GET", nil, keys)
    local has_vault = result.has_vault

    if not has_vault then
        print("Vault exists: no")
        print("Variable count: 0")
    else
        print("Vault exists: yes")
        print("Variable count: " .. (result.count or 0))
        if result.updated_at then
            print("Last updated: " .. os.date("%Y-%m-%d %H:%M:%S", result.updated_at))
        end
    end
end

local function service_env_set(service_id, env_content, keys)
    if not env_content or env_content == "" then
        io.stderr:write(RED .. "Error: No environment content provided" .. RESET .. "\n")
        return false
    end

    if #env_content > MAX_ENV_CONTENT_SIZE then
        io.stderr:write(RED .. "Error: Environment content too large (max " .. MAX_ENV_CONTENT_SIZE .. " bytes)" .. RESET .. "\n")
        return false
    end

    local result = api_request_text("/services/" .. service_id .. "/env", "PUT", env_content, keys)

    if result.error then
        io.stderr:write(RED .. "Error: " .. result.error .. RESET .. "\n")
        return false
    end

    local count = result.count or 0
    local plural = count == 1 and "" or "s"
    print(GREEN .. "Environment vault updated: " .. count .. " variable" .. plural .. RESET)
    if result.message then print(result.message) end
    return true
end

local function service_env_export(service_id, keys)
    local result = api_request("/services/" .. service_id .. "/env/export", "POST", {}, keys)
    local env_content = result.env
    if env_content and env_content ~= "" then
        io.write(env_content)
        if not env_content:match("\n$") then print() end
    end
end

local function service_env_delete(service_id, keys)
    api_request("/services/" .. service_id .. "/env", "DELETE", nil, keys)
    print(GREEN .. "Environment vault deleted" .. RESET)
end

local function read_env_file_content(filepath)
    local file = io.open(filepath, "r")
    if not file then
        io.stderr:write(RED .. "Error: Env file not found: " .. filepath .. RESET .. "\n")
        os.exit(1)
    end
    local content = file:read("*all")
    file:close()
    return content
end

local function build_env_content(envs, env_file)
    local parts = {}

    -- Read from env file first
    if env_file and env_file ~= "" then
        table.insert(parts, read_env_file_content(env_file))
    end

    -- Add -e flags
    for _, e in ipairs(envs) do
        if e:find("=") then
            table.insert(parts, e)
        end
    end

    return table.concat(parts, "\n")
end

local function cmd_service_env(action, target, envs, env_file, keys)
    if not action or action == "" then
        io.stderr:write(RED .. "Error: env action required (status, set, export, delete)" .. RESET .. "\n")
        os.exit(1)
    end

    if not target or target == "" then
        io.stderr:write(RED .. "Error: Service ID required for env command" .. RESET .. "\n")
        os.exit(1)
    end

    if action == "status" then
        service_env_status(target, keys)
    elseif action == "set" then
        local env_content = build_env_content(envs, env_file)
        if env_content == "" then
            io.stderr:write(RED .. "Error: No env content provided. Use -e KEY=VAL or --env-file" .. RESET .. "\n")
            os.exit(1)
        end
        service_env_set(target, env_content, keys)
    elseif action == "export" then
        service_env_export(target, keys)
    elseif action == "delete" then
        service_env_delete(target, keys)
    else
        io.stderr:write(RED .. "Error: Unknown env action '" .. action .. "'. Use: status, set, export, delete" .. RESET .. "\n")
        os.exit(1)
    end
end

local function read_file(filename)
    local file, err = io.open(filename, "rb")
    if not file then
        io.stderr:write(RED .. "Error: File not found: " .. filename .. RESET .. "\n")
        os.exit(1)
    end
    local content = file:read("*all")
    file:close()
    return content
end

local function base64_encode(data)
    local b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    return ((data:gsub('.', function(x)
        local r,b='',x:byte()
        for i=8,1,-1 do r=r..(b%2^i-b%2^(i-1)>0 and '1' or '0') end
        return r;
    end)..'0000'):gsub('%d%d%d?%d?%d?%d?', function(x)
        if (#x < 6) then return '' end
        local c=0
        for i=1,6 do c=c+(x:sub(i,i)=='1' and 2^(6-i) or 0) end
        return b64:sub(c+1,c+1)
    end)..({ '', '==', '=' })[#data%3+1])
end

local function base64_decode(data)
    local b64 = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
    data = string.gsub(data, '[^'..b64..'=]', '')
    return (data:gsub('.', function(x)
        if (x == '=') then return '' end
        local r,f='',(b64:find(x)-1)
        for i=6,1,-1 do r=r..(f%2^i-f%2^(i-1)>0 and '1' or '0') end
        return r;
    end):gsub('%d%d%d?%d?%d?%d?%d?%d?', function(x)
        if (#x ~= 8) then return '' end
        local c=0
        for i=1,8 do c=c+(x:sub(i,i)=='1' and 2^(8-i) or 0) end
        return string.char(c)
    end))
end

local function file_exists(filename)
    local file = io.open(filename, "r")
    if file then
        file:close()
        return true
    end
    return false
end

local function cmd_execute(options)
    local keys = get_api_keys(options.api_key)
    local code
    local language

    -- Check for inline mode: -s/--shell specified, or source_file doesn't exist
    if options.exec_shell then
        -- Inline mode with specified language
        code = options.source_file
        language = options.exec_shell
    elseif not file_exists(options.source_file) then
        -- File doesn't exist - treat as inline bash code
        code = options.source_file
        language = "bash"
    else
        -- Normal file execution
        code = read_file(options.source_file)
        language = detect_language(options.source_file)
    end

    local payload = { language = language, code = code }

    if options.env and #options.env > 0 then
        local env_vars = {}
        for _, e in ipairs(options.env) do
            local k, v = e:match("^([^=]+)=(.*)$")
            if k and v then
                env_vars[k] = v
            end
        end
        if next(env_vars) then
            payload.env = env_vars
        end
    end

    if options.files and #options.files > 0 then
        local input_files = {}
        for _, filepath in ipairs(options.files) do
            local content = read_file(filepath)
            table.insert(input_files, {
                filename = filepath:match("([^/]+)$"),
                content_base64 = base64_encode(content)
            })
        end
        payload.input_files = input_files
    end

    if options.artifacts then payload.return_artifacts = true end
    if options.network then payload.network = options.network end
    if options.vcpu then payload.vcpu = options.vcpu end

    local result = api_request("/execute", "POST", payload, keys)

    if result.stdout then
        io.write(BLUE .. result.stdout .. RESET)
    end
    if result.stderr then
        io.stderr:write(RED .. result.stderr .. RESET)
    end

    if options.artifacts and result.artifacts then
        local out_dir = options.output_dir or "."
        os.execute("mkdir -p " .. shell_escape(out_dir))
        for _, artifact in ipairs(result.artifacts) do
            local filename = artifact.filename or "artifact"
            local content = base64_decode(artifact.content_base64)
            local filepath = out_dir .. "/" .. filename
            local file = io.open(filepath, "wb")
            file:write(content)
            file:close()
            os.execute("chmod 755 " .. shell_escape(filepath))
            io.stderr:write(GREEN .. "Saved: " .. filepath .. RESET .. "\n")
        end
    end

    os.exit(result.exit_code or 0)
end

local function cmd_session(options)
    local keys = get_api_keys(options.api_key)

    if options.list then
        local result = api_request("/sessions", "GET", nil, keys)
        local sessions = result.sessions or {}
        if #sessions == 0 then
            print("No active sessions")
        else
            print(string.format("%-40s %-10s %-10s %s", "ID", "Shell", "Status", "Created"))
            for _, s in ipairs(sessions) do
                print(string.format("%-40s %-10s %-10s %s",
                    s.id or "N/A", s.shell or "N/A",
                    s.status or "N/A", s.created_at or "N/A"))
            end
        end
        return
    end

    if options.kill then
        api_request("/sessions/" .. options.kill, "DELETE", nil, keys)
        print(GREEN .. "Session terminated: " .. options.kill .. RESET)
        return
    end

    if options.attach then
        print(YELLOW .. "Attaching to session " .. options.attach .. "..." .. RESET)
        print(YELLOW .. "(Interactive sessions require WebSocket - use un2 for full support)" .. RESET)
        return
    end

    local payload = { shell = options.shell or "bash" }
    if options.network then payload.network = options.network end
    if options.vcpu then payload.vcpu = options.vcpu end
    if options.tmux then payload.persistence = "tmux" end
    if options.screen then payload.persistence = "screen" end
    if options.audit then payload.audit = true end

    -- Add input files
    if options.files and #options.files > 0 then
        local input_files = {}
        for _, filepath in ipairs(options.files) do
            local content = read_file(filepath)
            table.insert(input_files, {
                filename = filepath:match("([^/]+)$"),
                content_base64 = base64_encode(content)
            })
        end
        payload.input_files = input_files
    end

    print(YELLOW .. "Creating session..." .. RESET)
    local result = api_request("/sessions", "POST", payload, keys)
    print(GREEN .. "Session created: " .. (result.id or "N/A") .. RESET)
    print(YELLOW .. "(Interactive sessions require WebSocket - use un2 for full support)" .. RESET)
end

local function cmd_key(options)
    local keys = get_api_keys(options.api_key)

    if options.extend then
        -- Get public_key from validation response
        local url = PORTAL_BASE .. "/keys/validate"
        local tmpfile = os.tmpname()

        local timestamp = tostring(os.time())
        local body = ""
        local path = "/keys/validate"
        local message = timestamp .. ":POST:" .. path .. ":" .. body
        local msg_tmpfile = os.tmpname()

        local f = io.open(msg_tmpfile, "w")
        f:write(message)
        f:close()

        local hmac_cmd = "openssl dgst -sha256 -hmac " .. shell_escape(keys.secret_key) .. " -hex " .. shell_escape(msg_tmpfile) .. " | awk '{print $2}'"
        local sig_handle = io.popen(hmac_cmd)
        local signature = sig_handle:read("*a"):gsub("%s+$", "")
        sig_handle:close()
        os.remove(msg_tmpfile)

        local cmd = "curl -s -X POST " .. shell_escape(url) ..
                    " -H 'Authorization: Bearer " .. keys.public_key .. "'" ..
                    " -H 'X-Timestamp: " .. timestamp .. "'" ..
                    " -H 'X-Signature: " .. signature .. "'" ..
                    " -H 'Content-Type: application/json'" ..
                    " -w '\\n%{http_code}' -o " .. shell_escape(tmpfile)

        local handle = io.popen(cmd)
        local http_code = handle:read("*a"):match("(%d+)$")
        handle:close()

        local file = io.open(tmpfile, "r")
        local response = file:read("*all")
        file:close()
        os.remove(tmpfile)

        if not http_code or tonumber(http_code) < 200 or tonumber(http_code) >= 300 then
            io.stderr:write(RED .. "Error: HTTP " .. (http_code or "000") .. " - " .. response .. RESET .. "\n")
            os.exit(1)
        end

        local result = json.decode(response)
        local public_key = result.public_key

        if not public_key then
            io.stderr:write(RED .. "Error: Could not retrieve public key" .. RESET .. "\n")
            os.exit(1)
        end

        -- Open browser with extend URL
        local extend_url = PORTAL_BASE .. "/keys/extend?pk=" .. public_key
        print(GREEN .. "Opening browser to extend key..." .. RESET)
        print(extend_url)
        os.execute("xdg-open " .. shell_escape(extend_url) .. " 2>/dev/null || open " .. shell_escape(extend_url) .. " 2>/dev/null")
        return
    end

    -- Validate key (default action)
    local url = PORTAL_BASE .. "/keys/validate"
    local tmpfile = os.tmpname()

    local timestamp = tostring(os.time())
    local body = ""
    local path = "/keys/validate"
    local message = timestamp .. ":POST:" .. path .. ":" .. body
    local msg_tmpfile = os.tmpname()

    local f = io.open(msg_tmpfile, "w")
    f:write(message)
    f:close()

    local hmac_cmd = "openssl dgst -sha256 -hmac " .. shell_escape(keys.secret_key) .. " -hex " .. shell_escape(msg_tmpfile) .. " | awk '{print $2}'"
    local sig_handle = io.popen(hmac_cmd)
    local signature = sig_handle:read("*a"):gsub("%s+$", "")
    sig_handle:close()
    os.remove(msg_tmpfile)

    local cmd = "curl -s -X POST " .. shell_escape(url) ..
                " -H 'Authorization: Bearer " .. keys.public_key .. "'" ..
                " -H 'X-Timestamp: " .. timestamp .. "'" ..
                " -H 'X-Signature: " .. signature .. "'" ..
                " -H 'Content-Type: application/json'" ..
                " -w '\\n%{http_code}' -o " .. shell_escape(tmpfile)

    local handle = io.popen(cmd)
    local http_code = handle:read("*a"):match("(%d+)$")
    handle:close()

    local file = io.open(tmpfile, "r")
    local response = file:read("*all")
    file:close()
    os.remove(tmpfile)

    if not http_code or tonumber(http_code) < 200 or tonumber(http_code) >= 300 then
        io.stderr:write(RED .. "Error: Invalid API key" .. RESET .. "\n")
        os.exit(1)
    end

    local result = json.decode(response)

    if result.status == "valid" then
        print(GREEN .. "Valid" .. RESET)
        if result.public_key then print("Public Key: " .. result.public_key) end
        if result.tier then print("Tier: " .. result.tier) end
        if result.expires_at then print("Expires: " .. result.expires_at) end
    elseif result.status == "expired" then
        print(RED .. "Expired" .. RESET)
        if result.public_key then print("Public Key: " .. result.public_key) end
        if result.tier then print("Tier: " .. result.tier) end
        if result.expired_at then print("Expired: " .. result.expired_at) end
        print(YELLOW .. "To renew: Visit https://unsandbox.com/keys/extend" .. RESET)
    else
        print(RED .. "Invalid" .. RESET)
        if result.message then print("Message: " .. result.message) end
    end
end

local function cmd_service(options)
    local keys = get_api_keys(options.api_key)

    if options.list then
        local result = api_request("/services", "GET", nil, keys)
        local services = result.services or {}
        if #services == 0 then
            print("No services")
        else
            print(string.format("%-20s %-15s %-10s %-15s %s", "ID", "Name", "Status", "Ports", "Domains"))
            for _, s in ipairs(services) do
                local ports = table.concat(s.ports or {}, ",")
                local domains = table.concat(s.domains or {}, ",")
                print(string.format("%-20s %-15s %-10s %-15s %s",
                    s.id or "N/A", s.name or "N/A",
                    s.status or "N/A", ports, domains))
            end
        end
        return
    end

    if options.info then
        local result = api_request("/services/" .. options.info, "GET", nil, keys)
        print(json.encode(result))
        return
    end

    if options.logs then
        local result = api_request("/services/" .. options.logs .. "/logs", "GET", nil, keys)
        print(result.logs or "")
        return
    end

    if options.tail then
        local result = api_request("/services/" .. options.tail .. "/logs?lines=9000", "GET", nil, keys)
        print(result.logs or "")
        return
    end

    if options.sleep then
        api_request("/services/" .. options.sleep .. "/sleep", "POST", nil, keys)
        print(GREEN .. "Service sleeping: " .. options.sleep .. RESET)
        return
    end

    if options.wake then
        api_request("/services/" .. options.wake .. "/wake", "POST", nil, keys)
        print(GREEN .. "Service waking: " .. options.wake .. RESET)
        return
    end

    if options.destroy then
        api_request("/services/" .. options.destroy, "DELETE", nil, keys)
        print(GREEN .. "Service destroyed: " .. options.destroy .. RESET)
        return
    end

    if options.resize then
        local vcpu = options.resize_vcpu or options.vcpu
        if not vcpu then
            io.stderr:write(RED .. "Error: --resize requires --vcpu or -v" .. RESET .. "\n")
            os.exit(1)
        end
        if vcpu < 1 or vcpu > 8 then
            io.stderr:write(RED .. "Error: vCPU must be between 1 and 8" .. RESET .. "\n")
            os.exit(1)
        end
        local payload = { vcpu = vcpu }
        api_request("/services/" .. options.resize, "PATCH", payload, keys)
        local ram = vcpu * 2
        print(GREEN .. "Service resized to " .. vcpu .. " vCPU, " .. ram .. " GB RAM" .. RESET)
        return
    end

    if options.execute then
        local payload = { command = options.command }
        local result = api_request("/services/" .. options.execute .. "/execute", "POST", payload, keys)
        if result.stdout then io.write(BLUE .. result.stdout .. RESET) end
        if result.stderr then io.stderr:write(RED .. result.stderr .. RESET) end
        return
    end

    if options.dump_bootstrap then
        io.stderr:write("Fetching bootstrap script from " .. options.dump_bootstrap .. "...\n")
        local payload = { command = "cat /tmp/bootstrap.sh" }
        local result = api_request("/services/" .. options.dump_bootstrap .. "/execute", "POST", payload, keys)

        if result.stdout then
            local bootstrap = result.stdout
            if options.dump_file then
                -- Write to file
                local file = io.open(options.dump_file, "w")
                if not file then
                    io.stderr:write(RED .. "Error: Could not write to " .. options.dump_file .. RESET .. "\n")
                    os.exit(1)
                end
                file:write(bootstrap)
                file:close()
                os.execute("chmod 755 " .. options.dump_file)
                print("Bootstrap saved to " .. options.dump_file)
            else
                -- Print to stdout
                io.write(bootstrap)
            end
        else
            io.stderr:write(RED .. "Error: Failed to fetch bootstrap (service not running or no bootstrap file)" .. RESET .. "\n")
            os.exit(1)
        end
        return
    end

    if options.name then
        local payload = { name = options.name }
        if options.ports then
            local ports = {}
            for p in options.ports:gmatch("[^,]+") do
                table.insert(ports, tonumber(p))
            end
            payload.ports = ports
        end
        if options.domains then
            local domains = {}
            for d in options.domains:gmatch("[^,]+") do
                table.insert(domains, d)
            end
            payload.domains = domains
        end
        if options.type then
            payload.service_type = options.type
        end
        if options.bootstrap then
            payload.bootstrap = options.bootstrap
        end
        if options.bootstrap_file then
            local file = io.open(options.bootstrap_file, "r")
            if not file then
                io.stderr:write(RED .. "Error: Bootstrap file not found: " .. options.bootstrap_file .. RESET .. "\n")
                os.exit(1)
            end
            payload.bootstrap_content = file:read("*all")
            file:close()
        end
        -- Add input files
        if options.files and #options.files > 0 then
            local input_files = {}
            for _, filepath in ipairs(options.files) do
                local content = read_file(filepath)
                table.insert(input_files, {
                    filename = filepath:match("([^/]+)$"),
                    content_base64 = base64_encode(content)
                })
            end
            payload.input_files = input_files
        end
        if options.network then payload.network = options.network end
        if options.vcpu then payload.vcpu = options.vcpu end

        local result = api_request("/services", "POST", payload, keys)
        local service_id = result.id
        print(GREEN .. "Service created: " .. (service_id or "N/A") .. RESET)
        print("Name: " .. (result.name or "N/A"))
        if result.url then print("URL: " .. result.url) end

        -- Auto-set vault if -e or --env-file provided
        local env_content = build_env_content(options.env or {}, options.env_file)
        if env_content ~= "" and service_id then
            service_env_set(service_id, env_content, keys)
        end
        return
    end

    io.stderr:write(RED .. "Error: Specify --name to create a service, or use --list, --info, etc." .. RESET .. "\n")
    os.exit(1)
end

local function main()
    local options = {
        command = nil,
        source_file = nil,
        env = {},
        files = {},
        artifacts = false,
        output_dir = nil,
        network = nil,
        vcpu = nil,
        api_key = nil,
        shell = nil,
        list = false,
        attach = nil,
        kill = nil,
        audit = false,
        tmux = false,
        screen = false,
        name = nil,
        ports = nil,
        domains = nil,
        type = nil,
        bootstrap = nil,
        bootstrap_file = nil,
        info = nil,
        logs = nil,
        tail = nil,
        sleep = nil,
        wake = nil,
        destroy = nil,
        resize = nil,
        resize_vcpu = nil,
        execute = nil,
        command = nil,
        dump_bootstrap = nil,
        dump_file = nil,
        extend = false,
        exec_shell = nil,
        env_file = nil,
        env_action = nil,
        env_target = nil
    }

    local i = 1
    while i <= #arg do
        local a = arg[i]

        if a == "session" or a == "service" or a == "key" then
            options.command = a
        elseif a == "-e" then
            i = i + 1
            table.insert(options.env, arg[i])
        elseif a == "-f" then
            i = i + 1
            table.insert(options.files, arg[i])
        elseif a == "-a" then
            options.artifacts = true
        elseif a == "-o" then
            i = i + 1
            options.output_dir = arg[i]
        elseif a == "-n" then
            i = i + 1
            options.network = arg[i]
        elseif a == "-v" then
            i = i + 1
            options.vcpu = tonumber(arg[i])
        elseif a == "-k" then
            i = i + 1
            options.api_key = arg[i]
        elseif a == "-s" or a == "--shell" then
            i = i + 1
            -- For session command, this is shell type. For execute, it's inline exec language.
            if options.command == "session" then
                options.shell = arg[i]
            else
                options.exec_shell = arg[i]
            end
        elseif a == "-l" or a == "--list" then
            options.list = true
        elseif a == "--attach" then
            i = i + 1
            options.attach = arg[i]
        elseif a == "--kill" then
            i = i + 1
            options.kill = arg[i]
        elseif a == "--audit" then
            options.audit = true
        elseif a == "--tmux" then
            options.tmux = true
        elseif a == "--screen" then
            options.screen = true
        elseif a == "--name" then
            i = i + 1
            options.name = arg[i]
        elseif a == "--ports" then
            i = i + 1
            options.ports = arg[i]
        elseif a == "--domains" then
            i = i + 1
            options.domains = arg[i]
        elseif a == "--type" then
            i = i + 1
            options.type = arg[i]
        elseif a == "--bootstrap" then
            i = i + 1
            options.bootstrap = arg[i]
        elseif a == "--bootstrap-file" then
            i = i + 1
            options.bootstrap_file = arg[i]
        elseif a == "--env-file" then
            i = i + 1
            options.env_file = arg[i]
        elseif a == "env" then
            -- Handle "service env <action> <target>" subcommand
            if options.command == "service" then
                i = i + 1
                if i <= #arg then
                    options.env_action = arg[i]
                end
                i = i + 1
                if i <= #arg and not arg[i]:match("^%-") then
                    options.env_target = arg[i]
                else
                    i = i - 1 -- back up if next arg is a flag
                end
            end
        elseif a == "--info" then
            i = i + 1
            options.info = arg[i]
        elseif a == "--logs" then
            i = i + 1
            options.logs = arg[i]
        elseif a == "--tail" then
            i = i + 1
            options.tail = arg[i]
        elseif a == "--freeze" then
            i = i + 1
            options.sleep = arg[i]
        elseif a == "--unfreeze" then
            i = i + 1
            options.wake = arg[i]
        elseif a == "--destroy" then
            i = i + 1
            options.destroy = arg[i]
        elseif a == "--resize" then
            i = i + 1
            options.resize = arg[i]
        elseif a == "--vcpu" then
            i = i + 1
            options.resize_vcpu = tonumber(arg[i])
        elseif a == "--execute" then
            i = i + 1
            options.execute = arg[i]
        elseif a == "--command" then
            i = i + 1
            options.command = arg[i]
        elseif a == "--dump-bootstrap" then
            i = i + 1
            options.dump_bootstrap = arg[i]
        elseif a == "--dump-file" then
            i = i + 1
            options.dump_file = arg[i]
        elseif a == "--extend" then
            options.extend = true
        elseif a:match("^%-") then
            io.stderr:write(RED .. "Unknown option: " .. a .. RESET .. "\n")
            os.exit(1)
        else
            options.source_file = a
        end

        i = i + 1
    end

    if options.command == "session" then
        cmd_session(options)
    elseif options.command == "service" then
        -- Check for "service env" subcommand
        if options.env_action then
            local keys = get_api_keys(options.api_key)
            cmd_service_env(options.env_action, options.env_target, options.env, options.env_file, keys)
        else
            cmd_service(options)
        end
    elseif options.command == "key" then
        cmd_key(options)
    elseif options.source_file then
        cmd_execute(options)
    else
        print([[
Unsandbox CLI - Execute code in secure sandboxes

Usage:
  ]] .. arg[0] .. [[ [options] <source_file>
  ]] .. arg[0] .. [[ session [options]
  ]] .. arg[0] .. [[ service [options]
  ]] .. arg[0] .. [[ key [options]

Execute options:
  -e KEY=VALUE      Environment variable (multiple allowed)
  -f FILE          Input file (multiple allowed)
  -a               Return artifacts
  -o DIR           Output directory for artifacts
  -n MODE          Network mode (zerotrust|semitrusted)
  -v N             vCPU count (1-8)
  -k KEY           API key

Session options:
  -s, --shell NAME  Shell/REPL (default: bash)
  -l, --list       List sessions
  --attach ID      Attach to session
  --kill ID        Terminate session
  --audit          Record session
  --tmux           Enable tmux persistence
  --screen         Enable screen persistence

Service options:
  --name NAME      Service name
  --ports PORTS    Comma-separated ports
  --domains DOMAINS Custom domains
  --type TYPE      Service type (minecraft|mumble|teamspeak|source|tcp|udp)
  --bootstrap CMD  Bootstrap command or URI
  --bootstrap-file FILE  Upload local file as bootstrap script
  -l, --list       List services
  --info ID        Get service details
  --logs ID        Get all logs
  --tail ID        Get last 9000 lines
  --freeze ID       Freeze service
  --unfreeze ID        Unfreeze service
  --destroy ID     Destroy service
  --resize ID      Resize service (requires --vcpu)
  --execute ID     Execute command in service
  --command CMD    Command to execute (with --execute)
  --dump-bootstrap ID  Dump bootstrap script
  --dump-file FILE     File to save bootstrap (with --dump-bootstrap)

Key options:
  --extend         Open browser to extend/renew key
]])
        os.exit(1)
    end
end

main()
