#!/usr/bin/env lua
-- PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
--
-- unsandbox.com Lua SDK (Synchronous)
-- Full API with execution, sessions, services, snapshots, and images.
--
-- Library Usage:
--     local Un = require("un")
--     local result = Un.execute("python", "print(42)")
--     print(result.stdout)
--
-- CLI Usage:
--     lua un.lua script.py
--     lua un.lua -s python 'print(42)'
--     lua un.lua session --list
--     lua un.lua service --list
--
-- Copyright 2025 TimeHexOn & foxhop & russell@unturf

local json = require("json")
local http = require("socket.http")
local https = require("ssl.https")
local ltn12 = require("ltn12")
local mime = require("mime")

local Un = {}
Un.API_BASE = "https://api.unsandbox.com"
Un.PORTAL_BASE = "https://unsandbox.com"
Un.VERSION = "4.3.2"
Un.LAST_ERROR = ""

-- Colors
local BLUE = "\027[34m"
local RED = "\027[31m"
local GREEN = "\027[32m"
local YELLOW = "\027[33m"
local RESET = "\027[0m"

-- Extension to language mapping
local EXT_MAP = {
    py = "python", js = "javascript", ts = "typescript",
    rb = "ruby", php = "php", pl = "perl", lua = "lua",
    sh = "bash", go = "go", rs = "rust", c = "c",
    cpp = "cpp", cc = "cpp", cxx = "cpp",
    java = "java", kt = "kotlin", cs = "csharp", fs = "fsharp",
    hs = "haskell", ml = "ocaml", clj = "clojure", scm = "scheme",
    lisp = "commonlisp", erl = "erlang", ex = "elixir", exs = "elixir",
    jl = "julia", r = "r", cr = "crystal",
    d = "d", nim = "nim", zig = "zig", v = "v",
    dart = "dart", groovy = "groovy", scala = "scala",
    f90 = "fortran", f95 = "fortran", cob = "cobol",
    pro = "prolog", forth = "forth", ["4th"] = "forth",
    tcl = "tcl", raku = "raku", m = "objc"
}

-- ============================================================================
-- Utility Functions
-- ============================================================================

function Un.version()
    return Un.VERSION
end

function Un.last_error()
    return Un.LAST_ERROR
end

function Un.set_error(msg)
    Un.LAST_ERROR = msg or ""
end

function Un.detect_language(filename)
    if not filename then return nil end
    local ext = filename:match("%.([^%.]+)$")
    return ext and EXT_MAP[ext:lower()] or nil
end

function Un.hmac_sign(secret, message)
    if not secret or not message then return nil end
    -- Use openssl for HMAC-SHA256
    local cmd = "echo -n '" .. message:gsub("'", "'\\''") .. "' | openssl dgst -sha256 -hmac '" .. secret:gsub("'", "'\\''") .. "' | sed 's/^.* //'"
    local handle = io.popen(cmd)
    local result = handle:read("*a")
    handle:close()
    return result and result:gsub("%s+", "") or nil
end

-- ============================================================================
-- Credential Management
-- ============================================================================

function Un.load_accounts_csv(path)
    path = path or (os.getenv("HOME") .. "/.unsandbox/accounts.csv")
    local file = io.open(path, "r")
    if not file then return {} end

    local accounts = {}
    for line in file:lines() do
        line = line:match("^%s*(.-)%s*$")
        if line ~= "" and not line:match("^#") then
            local pk, sk = line:match("([^,]+),(.+)")
            if pk and sk then
                table.insert(accounts, {pk, sk})
            end
        end
    end
    file:close()
    return accounts
end

function Un.get_credentials(opts)
    opts = opts or {}

    -- Tier 1: Arguments
    if opts.public_key and opts.secret_key then
        return opts.public_key, opts.secret_key
    end

    -- Tier 2: Environment
    local pk = os.getenv("UNSANDBOX_PUBLIC_KEY")
    local sk = os.getenv("UNSANDBOX_SECRET_KEY")
    if pk and sk then return pk, sk end

    -- Legacy fallback
    if os.getenv("UNSANDBOX_API_KEY") then
        return os.getenv("UNSANDBOX_API_KEY"), ""
    end

    -- Tier 3: Home directory
    local accounts = Un.load_accounts_csv()
    if #accounts > 0 then return accounts[1][1], accounts[1][2] end

    -- Tier 4: Local directory
    accounts = Un.load_accounts_csv("./accounts.csv")
    if #accounts > 0 then return accounts[1][1], accounts[1][2] end

    Un.set_error("No credentials found")
    return nil, nil
end

-- ============================================================================
-- API Communication
-- ============================================================================

function Un.api_request(method, endpoint, body, opts, extra_headers)
    opts = opts or {}
    extra_headers = extra_headers or {}
    local pk, sk = Un.get_credentials(opts)

    if not pk then
        Un.set_error("No credentials available")
        return nil
    end

    local timestamp = tostring(os.time())
    local url = Un.API_BASE .. endpoint
    local body_str = ""
    if body then
        if type(body) == "table" then
            body_str = json.encode(body)
        else
            body_str = body
        end
    end

    local content_type = opts.content_type or "application/json"
    local signature = ""
    if sk and sk ~= "" then
        signature = Un.hmac_sign(sk, timestamp .. ":" .. method .. ":" .. endpoint .. ":" .. body_str)
    end

    local headers = {
        ["Authorization"] = "Bearer " .. pk,
        ["Content-Type"] = content_type
    }

    if signature then
        headers["X-Timestamp"] = timestamp
        headers["X-Signature"] = signature
    end

    -- Add extra headers
    for k, v in pairs(extra_headers) do
        headers[k] = v
    end

    local resp_body = {}
    local resp, status = https.request({
        url = url,
        method = method,
        headers = headers,
        source = body_str ~= "" and ltn12.source.string(body_str) or nil,
        sink = ltn12.sink.table(resp_body)
    })

    if status ~= 200 and status ~= 201 then
        Un.set_error("API error (" .. tostring(status) .. ")")
        return nil, status
    end

    local response_text = table.concat(resp_body)
    if response_text and response_text ~= "" then
        local ok, result = pcall(json.decode, response_text)
        if ok then return result, status end
    end
    return { success = true }, status
end

function Un.api_request_with_sudo(method, endpoint, body, opts)
    opts = opts or {}
    local pk, sk = Un.get_credentials(opts)

    local timestamp = tostring(os.time())
    local body_str = body and json.encode(body) or ""

    local signature = ""
    if sk and sk ~= "" then
        signature = Un.hmac_sign(sk, timestamp .. ":" .. method .. ":" .. endpoint .. ":" .. body_str)
    end

    local headers = {
        ["Authorization"] = "Bearer " .. pk,
        ["Content-Type"] = "application/json"
    }

    if signature then
        headers["X-Timestamp"] = timestamp
        headers["X-Signature"] = signature
    end

    local resp_body = {}
    local resp, status = https.request({
        url = Un.API_BASE .. endpoint,
        method = method,
        headers = headers,
        source = body_str ~= "" and ltn12.source.string(body_str) or nil,
        sink = ltn12.sink.table(resp_body)
    })

    -- Handle 428 - Sudo OTP required
    if status == 428 then
        local response_text = table.concat(resp_body)
        local response_data = {}
        pcall(function() response_data = json.decode(response_text) end)
        local challenge_id = response_data.challenge_id or ""

        io.stderr:write(YELLOW .. "Confirmation required. Check your email for a one-time code." .. RESET .. "\n")
        io.stderr:write("Enter OTP: ")
        io.stderr:flush()

        local otp = io.read("*line")
        if not otp or otp == "" then
            Un.set_error("Operation cancelled")
            return nil
        end

        local extra = { ["X-Sudo-OTP"] = otp }
        if challenge_id ~= "" then
            extra["X-Sudo-Challenge"] = challenge_id
        end

        return Un.api_request(method, endpoint, body, opts, extra)
    end

    if status ~= 200 and status ~= 201 then
        Un.set_error("API error (" .. tostring(status) .. ")")
        return nil
    end

    local response_text = table.concat(resp_body)
    if response_text and response_text ~= "" then
        local ok, result = pcall(json.decode, response_text)
        if ok then return result end
    end
    return { success = true }
end

-- ============================================================================
-- Execution Functions (8)
-- ============================================================================

function Un.execute(language, code, opts)
    opts = opts or {}
    local body = {
        language = language,
        code = code,
        network_mode = opts.network_mode or "zerotrust",
        ttl = opts.ttl or 60
    }
    if opts.env then body.env = opts.env end
    if opts.input_files then body.input_files = opts.input_files end
    if opts.return_artifacts then body.return_artifacts = true end
    return Un.api_request("POST", "/execute", body, opts)
end

function Un.execute_async(language, code, opts)
    opts = opts or {}
    local body = {
        language = language,
        code = code,
        network_mode = opts.network_mode or "zerotrust",
        ttl = opts.ttl or 300
    }
    return Un.api_request("POST", "/execute/async", body, opts)
end

function Un.wait_job(job_id, opts)
    opts = opts or {}
    local delays = {300, 450, 700, 900, 650, 1600, 2000}

    for i = 0, 119 do
        local job = Un.get_job(job_id, opts)
        if job and job.status == "completed" then return job end
        if job and job.status == "failed" then
            Un.set_error("Job failed")
            return nil
        end

        local delay = delays[(i % 7) + 1] or 2000
        require("socket").sleep(delay / 1000)
    end

    Un.set_error("Max polls exceeded")
    return nil
end

function Un.get_job(job_id, opts)
    return Un.api_request("GET", "/jobs/" .. job_id, nil, opts)
end

function Un.cancel_job(job_id, opts)
    return Un.api_request("DELETE", "/jobs/" .. job_id, nil, opts)
end

function Un.list_jobs(opts)
    return Un.api_request("GET", "/jobs", nil, opts)
end

function Un.get_languages(opts)
    opts = opts or {}
    local cache_ttl = opts.cache_ttl or 3600
    local cache_path = os.getenv("HOME") .. "/.unsandbox/languages.json"

    -- Try cache
    local file = io.open(cache_path, "r")
    if file then
        local content = file:read("*a")
        file:close()
        local ok, cached = pcall(json.decode, content)
        if ok and cached and cached.timestamp then
            if os.time() - cached.timestamp < cache_ttl then
                return cached.languages
            end
        end
    end

    -- Fetch from API
    local result = Un.api_request("GET", "/languages", nil, opts)
    local langs = result and result.languages or {}

    -- Save to cache
    os.execute("mkdir -p " .. os.getenv("HOME") .. "/.unsandbox")
    file = io.open(cache_path, "w")
    if file then
        file:write(json.encode({ languages = langs, timestamp = os.time() }))
        file:close()
    end

    return langs
end

-- ============================================================================
-- Session Functions (9)
-- ============================================================================

function Un.session_list(opts)
    return Un.api_request("GET", "/sessions", nil, opts)
end

function Un.session_get(session_id, opts)
    return Un.api_request("GET", "/sessions/" .. session_id, nil, opts)
end

function Un.session_create(opts)
    opts = opts or {}
    local body = { shell = opts.shell or "bash" }
    if opts.network then body.network = opts.network end
    if opts.vcpu then body.vcpu = opts.vcpu end
    if opts.input_files then body.input_files = opts.input_files end
    if opts.persistence then body.persistence = opts.persistence end
    return Un.api_request("POST", "/sessions", body, opts)
end

function Un.session_destroy(session_id, opts)
    return Un.api_request("DELETE", "/sessions/" .. session_id, nil, opts)
end

function Un.session_freeze(session_id, opts)
    return Un.api_request("POST", "/sessions/" .. session_id .. "/freeze", {}, opts)
end

function Un.session_unfreeze(session_id, opts)
    return Un.api_request("POST", "/sessions/" .. session_id .. "/unfreeze", {}, opts)
end

function Un.session_boost(session_id, vcpu, opts)
    return Un.api_request("POST", "/sessions/" .. session_id .. "/boost", { vcpu = vcpu }, opts)
end

function Un.session_unboost(session_id, opts)
    return Un.api_request("POST", "/sessions/" .. session_id .. "/unboost", {}, opts)
end

function Un.session_execute(session_id, command, opts)
    return Un.api_request("POST", "/sessions/" .. session_id .. "/execute", { command = command }, opts)
end

-- ============================================================================
-- Service Functions (17)
-- ============================================================================

function Un.service_list(opts)
    return Un.api_request("GET", "/services", nil, opts)
end

function Un.service_get(service_id, opts)
    return Un.api_request("GET", "/services/" .. service_id, nil, opts)
end

function Un.service_create(opts)
    opts = opts or {}
    local body = { name = opts.name }
    if opts.ports then body.ports = opts.ports end
    if opts.domains then body.domains = opts.domains end
    if opts.bootstrap then body.bootstrap = opts.bootstrap end
    if opts.bootstrap_content then body.bootstrap_content = opts.bootstrap_content end
    if opts.network then body.network = opts.network end
    if opts.vcpu then body.vcpu = opts.vcpu end
    if opts.service_type then body.service_type = opts.service_type end
    if opts.input_files then body.input_files = opts.input_files end
    if opts.unfreeze_on_demand then body.unfreeze_on_demand = true end
    return Un.api_request("POST", "/services", body, opts)
end

function Un.service_destroy(service_id, opts)
    return Un.api_request_with_sudo("DELETE", "/services/" .. service_id, nil, opts)
end

function Un.service_freeze(service_id, opts)
    return Un.api_request("POST", "/services/" .. service_id .. "/freeze", {}, opts)
end

function Un.service_unfreeze(service_id, opts)
    return Un.api_request("POST", "/services/" .. service_id .. "/unfreeze", {}, opts)
end

function Un.service_lock(service_id, opts)
    return Un.api_request("POST", "/services/" .. service_id .. "/lock", {}, opts)
end

function Un.service_unlock(service_id, opts)
    return Un.api_request_with_sudo("POST", "/services/" .. service_id .. "/unlock", {}, opts)
end

function Un.service_set_unfreeze_on_demand(service_id, enabled, opts)
    return Un.api_request("PATCH", "/services/" .. service_id, { unfreeze_on_demand = enabled }, opts)
end

function Un.service_redeploy(service_id, opts)
    opts = opts or {}
    local body = {}
    if opts.bootstrap then body.bootstrap = opts.bootstrap end
    return Un.api_request("POST", "/services/" .. service_id .. "/redeploy", body, opts)
end

function Un.service_logs(service_id, opts)
    opts = opts or {}
    local endpoint = "/services/" .. service_id .. "/logs"
    if opts.lines then endpoint = endpoint .. "?lines=" .. opts.lines end
    return Un.api_request("GET", endpoint, nil, opts)
end

function Un.service_execute(service_id, command, opts)
    opts = opts or {}
    local body = { command = command }
    if opts.timeout then body.timeout = opts.timeout end
    return Un.api_request("POST", "/services/" .. service_id .. "/execute", body, opts)
end

function Un.service_env_get(service_id, opts)
    return Un.api_request("GET", "/services/" .. service_id .. "/env", nil, opts)
end

function Un.service_env_set(service_id, env_content, opts)
    opts = opts or {}
    opts.content_type = "text/plain"
    return Un.api_request("PUT", "/services/" .. service_id .. "/env", env_content, opts)
end

function Un.service_env_delete(service_id, opts)
    return Un.api_request("DELETE", "/services/" .. service_id .. "/env", nil, opts)
end

function Un.service_env_export(service_id, opts)
    return Un.api_request("POST", "/services/" .. service_id .. "/env/export", {}, opts)
end

function Un.service_resize(service_id, vcpu, opts)
    return Un.api_request("PATCH", "/services/" .. service_id, { vcpu = vcpu }, opts)
end

-- ============================================================================
-- Snapshot Functions (9)
-- ============================================================================

function Un.snapshot_list(opts)
    return Un.api_request("GET", "/snapshots", nil, opts)
end

function Un.snapshot_get(snapshot_id, opts)
    return Un.api_request("GET", "/snapshots/" .. snapshot_id, nil, opts)
end

function Un.snapshot_session(session_id, opts)
    opts = opts or {}
    local body = {}
    if opts.name then body.name = opts.name end
    if opts.hot then body.hot = true end
    return Un.api_request("POST", "/sessions/" .. session_id .. "/snapshot", body, opts)
end

function Un.snapshot_service(service_id, opts)
    opts = opts or {}
    local body = {}
    if opts.name then body.name = opts.name end
    if opts.hot then body.hot = true end
    return Un.api_request("POST", "/services/" .. service_id .. "/snapshot", body, opts)
end

function Un.snapshot_restore(snapshot_id, opts)
    return Un.api_request("POST", "/snapshots/" .. snapshot_id .. "/restore", {}, opts)
end

function Un.snapshot_delete(snapshot_id, opts)
    return Un.api_request_with_sudo("DELETE", "/snapshots/" .. snapshot_id, nil, opts)
end

function Un.snapshot_lock(snapshot_id, opts)
    return Un.api_request("POST", "/snapshots/" .. snapshot_id .. "/lock", {}, opts)
end

function Un.snapshot_unlock(snapshot_id, opts)
    return Un.api_request_with_sudo("POST", "/snapshots/" .. snapshot_id .. "/unlock", {}, opts)
end

function Un.snapshot_clone(snapshot_id, opts)
    opts = opts or {}
    local body = { clone_type = opts.clone_type or "session" }
    if opts.name then body.name = opts.name end
    if opts.ports then body.ports = opts.ports end
    if opts.shell then body.shell = opts.shell end
    return Un.api_request("POST", "/snapshots/" .. snapshot_id .. "/clone", body, opts)
end

-- ============================================================================
-- Image Functions (13)
-- ============================================================================

function Un.image_list(opts)
    opts = opts or {}
    local endpoint = "/images"
    if opts.filter then endpoint = endpoint .. "?filter=" .. opts.filter end
    return Un.api_request("GET", endpoint, nil, opts)
end

function Un.image_get(image_id, opts)
    return Un.api_request("GET", "/images/" .. image_id, nil, opts)
end

function Un.image_publish(opts)
    opts = opts or {}
    local body = {
        source_type = opts.source_type,
        source_id = opts.source_id
    }
    if opts.name then body.name = opts.name end
    if opts.description then body.description = opts.description end
    return Un.api_request("POST", "/images/publish", body, opts)
end

function Un.image_delete(image_id, opts)
    return Un.api_request_with_sudo("DELETE", "/images/" .. image_id, nil, opts)
end

function Un.image_lock(image_id, opts)
    return Un.api_request("POST", "/images/" .. image_id .. "/lock", {}, opts)
end

function Un.image_unlock(image_id, opts)
    return Un.api_request_with_sudo("POST", "/images/" .. image_id .. "/unlock", {}, opts)
end

function Un.image_set_visibility(image_id, visibility, opts)
    return Un.api_request("POST", "/images/" .. image_id .. "/visibility", { visibility = visibility }, opts)
end

function Un.image_grant_access(image_id, trusted_api_key, opts)
    return Un.api_request("POST", "/images/" .. image_id .. "/access", { api_key = trusted_api_key }, opts)
end

function Un.image_revoke_access(image_id, trusted_api_key, opts)
    return Un.api_request("DELETE", "/images/" .. image_id .. "/access/" .. trusted_api_key, nil, opts)
end

function Un.image_list_trusted(image_id, opts)
    return Un.api_request("GET", "/images/" .. image_id .. "/access", nil, opts)
end

function Un.image_transfer(image_id, to_api_key, opts)
    return Un.api_request("POST", "/images/" .. image_id .. "/transfer", { to_api_key = to_api_key }, opts)
end

function Un.image_spawn(image_id, opts)
    opts = opts or {}
    local body = {}
    if opts.name then body.name = opts.name end
    if opts.ports then body.ports = opts.ports end
    if opts.bootstrap then body.bootstrap = opts.bootstrap end
    if opts.network_mode then body.network_mode = opts.network_mode end
    return Un.api_request("POST", "/images/" .. image_id .. "/spawn", body, opts)
end

function Un.image_clone(image_id, opts)
    opts = opts or {}
    local body = {}
    if opts.name then body.name = opts.name end
    if opts.description then body.description = opts.description end
    return Un.api_request("POST", "/images/" .. image_id .. "/clone", body, opts)
end

-- ============================================================================
-- PaaS Logs Functions (2)
-- ============================================================================

function Un.logs_fetch(opts)
    opts = opts or {}
    local body = {
        source = opts.source or "all",
        lines = opts.lines or 100,
        since = opts.since or "1h"
    }
    if opts.grep then body.grep = opts.grep end
    return Un.api_request("POST", "/paas/logs", body, opts)
end

function Un.logs_stream(opts)
    -- SSE streaming not easily supported in sync Lua
    Un.set_error("logs_stream requires async support")
    return nil
end

-- ============================================================================
-- Key Validation
-- ============================================================================

function Un.validate_keys(opts)
    opts = opts or {}
    local pk, sk = Un.get_credentials(opts)

    local timestamp = tostring(os.time())
    local signature = ""
    if sk and sk ~= "" then
        signature = Un.hmac_sign(sk, timestamp .. ":POST:/keys/validate:")
    end

    local headers = {
        ["Authorization"] = "Bearer " .. pk,
        ["Content-Type"] = "application/json"
    }

    if signature then
        headers["X-Timestamp"] = timestamp
        headers["X-Signature"] = signature
    end

    local resp_body = {}
    local resp, status = https.request({
        url = Un.PORTAL_BASE .. "/keys/validate",
        method = "POST",
        headers = headers,
        sink = ltn12.sink.table(resp_body)
    })

    if status == 200 then
        local response_text = table.concat(resp_body)
        local ok, result = pcall(json.decode, response_text)
        if ok then return result end
    end
    return nil
end

function Un.health_check(opts)
    local resp_body = {}
    local resp, status = https.request({
        url = Un.API_BASE .. "/health",
        method = "GET",
        sink = ltn12.sink.table(resp_body)
    })
    return status == 200
end

-- ============================================================================
-- CLI Implementation
-- ============================================================================

local function run_file(filename)
    local f = io.open(filename, "r")
    if not f then
        io.stderr:write(RED .. "Error: File not found: " .. filename .. RESET .. "\n")
        os.exit(1)
    end
    local code = f:read("*a")
    f:close()

    local lang = Un.detect_language(filename)
    if not lang then
        io.stderr:write(RED .. "Error: Cannot detect language" .. RESET .. "\n")
        os.exit(1)
    end

    local result = Un.execute(lang, code)
    if result then
        if result.stdout then print(result.stdout) end
        if result.stderr then io.stderr:write(result.stderr) end
        os.exit(result.exit_code or 0)
    else
        io.stderr:write(RED .. "Error: " .. Un.last_error() .. RESET .. "\n")
        os.exit(1)
    end
end

-- CLI entry point
if arg and arg[0] then
    local args = arg
    local i = 1

    if #args == 0 then
        print("Usage: lua un.lua [options] <source_file>")
        print("       lua un.lua -s <language> '<code>'")
        print("       lua un.lua session [options]")
        print("       lua un.lua service [options]")
        print("       lua un.lua snapshot [options]")
        print("       lua un.lua image [options]")
        print("       lua un.lua languages [--json]")
        print("       lua un.lua key [--extend]")
        os.exit(1)
    end

    local cmd = args[1]

    if cmd == "languages" then
        local json_output = args[2] == "--json"
        local langs = Un.get_languages()
        if json_output then
            print(json.encode(langs))
        else
            for _, lang in ipairs(langs) do
                print(lang)
            end
        end
    elseif cmd == "key" then
        local result = Un.validate_keys()
        if result then
            if result.expired then
                print(RED .. "Expired" .. RESET)
            else
                print(GREEN .. "Valid" .. RESET)
            end
            print("Public Key: " .. (result.public_key or "N/A"))
            print("Tier: " .. (result.tier or "N/A"))
            if result.expires_at then
                print("Expires: " .. result.expires_at)
            end
        else
            print(RED .. "Error: " .. Un.last_error() .. RESET)
        end
    elseif cmd == "session" then
        i = 2
        local action = nil
        local target = nil

        while i <= #args do
            if args[i] == "--list" or args[i] == "-l" then
                action = "list"
            elseif args[i] == "--info" then
                action = "info"
                i = i + 1
                target = args[i]
            elseif args[i] == "--kill" then
                action = "kill"
                i = i + 1
                target = args[i]
            elseif args[i] == "--freeze" then
                action = "freeze"
                i = i + 1
                target = args[i]
            elseif args[i] == "--unfreeze" then
                action = "unfreeze"
                i = i + 1
                target = args[i]
            end
            i = i + 1
        end

        if action == "list" then
            local result = Un.session_list()
            if result and result.sessions then
                if #result.sessions == 0 then
                    print("No active sessions")
                else
                    print(string.format("%-40s %-10s %-10s %s", "ID", "Shell", "Status", "Created"))
                    for _, s in ipairs(result.sessions) do
                        print(string.format("%-40s %-10s %-10s %s",
                            s.id or "N/A", s.shell or "N/A",
                            s.status or "N/A", s.created_at or "N/A"))
                    end
                end
            end
        elseif action == "info" then
            local result = Un.session_get(target)
            print(json.encode(result))
        elseif action == "kill" then
            Un.session_destroy(target)
            print(GREEN .. "Session terminated: " .. target .. RESET)
        elseif action == "freeze" then
            Un.session_freeze(target)
            print(GREEN .. "Session frozen: " .. target .. RESET)
        elseif action == "unfreeze" then
            Un.session_unfreeze(target)
            print(GREEN .. "Session unfreezing: " .. target .. RESET)
        else
            print("Usage: lua un.lua session --list|--info ID|--kill ID|--freeze ID|--unfreeze ID")
        end
    elseif cmd == "service" then
        i = 2
        local action = nil
        local target = nil

        while i <= #args do
            if args[i] == "--list" or args[i] == "-l" then
                action = "list"
            elseif args[i] == "--info" then
                action = "info"
                i = i + 1
                target = args[i]
            elseif args[i] == "--destroy" then
                action = "destroy"
                i = i + 1
                target = args[i]
            elseif args[i] == "--freeze" then
                action = "freeze"
                i = i + 1
                target = args[i]
            elseif args[i] == "--unfreeze" then
                action = "unfreeze"
                i = i + 1
                target = args[i]
            elseif args[i] == "--logs" then
                action = "logs"
                i = i + 1
                target = args[i]
            end
            i = i + 1
        end

        if action == "list" then
            local result = Un.service_list()
            if result and result.services then
                if #result.services == 0 then
                    print("No services")
                else
                    print(string.format("%-20s %-15s %-10s %-15s %s", "ID", "Name", "Status", "Ports", "Domains"))
                    for _, s in ipairs(result.services) do
                        local ports = table.concat(s.ports or {}, ",")
                        local domains = table.concat(s.domains or {}, ",")
                        print(string.format("%-20s %-15s %-10s %-15s %s",
                            s.id or "N/A", s.name or "N/A",
                            s.status or "N/A", ports, domains))
                    end
                end
            end
        elseif action == "info" then
            local result = Un.service_get(target)
            print(json.encode(result))
        elseif action == "destroy" then
            Un.service_destroy(target)
            print(GREEN .. "Service destroyed: " .. target .. RESET)
        elseif action == "freeze" then
            Un.service_freeze(target)
            print(GREEN .. "Service frozen: " .. target .. RESET)
        elseif action == "unfreeze" then
            Un.service_unfreeze(target)
            print(GREEN .. "Service unfreezing: " .. target .. RESET)
        elseif action == "logs" then
            local result = Un.service_logs(target)
            if result and result.logs then
                print(result.logs)
            end
        else
            print("Usage: lua un.lua service --list|--info ID|--destroy ID|--freeze ID|--unfreeze ID|--logs ID")
        end
    elseif cmd == "snapshot" then
        i = 2
        local action = nil
        local target = nil

        while i <= #args do
            if args[i] == "--list" or args[i] == "-l" then
                action = "list"
            elseif args[i] == "--info" then
                action = "info"
                i = i + 1
                target = args[i]
            elseif args[i] == "--delete" then
                action = "delete"
                i = i + 1
                target = args[i]
            elseif args[i] == "--restore" then
                action = "restore"
                i = i + 1
                target = args[i]
            end
            i = i + 1
        end

        if action == "list" then
            local result = Un.snapshot_list()
            if result and result.snapshots then
                if #result.snapshots == 0 then
                    print("No snapshots")
                else
                    print(string.format("%-40s %-20s %-10s %s", "ID", "Name", "Type", "Created"))
                    for _, s in ipairs(result.snapshots) do
                        print(string.format("%-40s %-20s %-10s %s",
                            s.id or "N/A", s.name or "N/A",
                            s.type or "N/A", s.created_at or "N/A"))
                    end
                end
            end
        elseif action == "info" then
            local result = Un.snapshot_get(target)
            print(json.encode(result))
        elseif action == "delete" then
            Un.snapshot_delete(target)
            print(GREEN .. "Snapshot deleted: " .. target .. RESET)
        elseif action == "restore" then
            Un.snapshot_restore(target)
            print(GREEN .. "Snapshot restored" .. RESET)
        else
            print("Usage: lua un.lua snapshot --list|--info ID|--delete ID|--restore ID")
        end
    elseif cmd == "image" then
        i = 2
        local action = nil
        local target = nil
        local name = nil
        local ports = nil
        local source_type = nil
        local visibility_mode = nil

        while i <= #args do
            if args[i] == "--list" or args[i] == "-l" then
                action = "list"
            elseif args[i] == "--info" then
                action = "info"
                i = i + 1
                target = args[i]
            elseif args[i] == "--delete" then
                action = "delete"
                i = i + 1
                target = args[i]
            elseif args[i] == "--lock" then
                action = "lock"
                i = i + 1
                target = args[i]
            elseif args[i] == "--unlock" then
                action = "unlock"
                i = i + 1
                target = args[i]
            elseif args[i] == "--publish" then
                action = "publish"
                i = i + 1
                target = args[i]
            elseif args[i] == "--source-type" then
                i = i + 1
                source_type = args[i]
            elseif args[i] == "--visibility" then
                action = "visibility"
                i = i + 1
                target = args[i]
                i = i + 1
                visibility_mode = args[i]
            elseif args[i] == "--spawn" then
                action = "spawn"
                i = i + 1
                target = args[i]
            elseif args[i] == "--clone" then
                action = "clone"
                i = i + 1
                target = args[i]
            elseif args[i] == "--name" then
                i = i + 1
                name = args[i]
            elseif args[i] == "--ports" then
                i = i + 1
                ports = {}
                for p in args[i]:gmatch("[^,]+") do
                    table.insert(ports, tonumber(p))
                end
            end
            i = i + 1
        end

        if action == "list" then
            local result = Un.image_list()
            if result and result.images then
                if #result.images == 0 then
                    print("No images")
                else
                    print(string.format("%-40s %-20s %-10s %s", "ID", "Name", "Visibility", "Created"))
                    for _, img in ipairs(result.images) do
                        print(string.format("%-40s %-20s %-10s %s",
                            img.id or "N/A", img.name or "N/A",
                            img.visibility or "N/A", img.created_at or "N/A"))
                    end
                end
            end
        elseif action == "info" then
            local result = Un.image_get(target)
            print(json.encode(result))
        elseif action == "delete" then
            Un.image_delete(target)
            print(GREEN .. "Image deleted: " .. target .. RESET)
        elseif action == "lock" then
            Un.image_lock(target)
            print(GREEN .. "Image locked: " .. target .. RESET)
        elseif action == "unlock" then
            Un.image_unlock(target)
            print(GREEN .. "Image unlocked: " .. target .. RESET)
        elseif action == "publish" then
            if not source_type then
                io.stderr:write(RED .. "Error: --source-type required" .. RESET .. "\n")
                os.exit(1)
            end
            local result = Un.image_publish({ source_type = source_type, source_id = target, name = name })
            print(GREEN .. "Image published" .. RESET)
            print(json.encode(result))
        elseif action == "visibility" then
            Un.image_set_visibility(target, visibility_mode)
            print(GREEN .. "Visibility set to " .. visibility_mode .. RESET)
        elseif action == "spawn" then
            local result = Un.image_spawn(target, { name = name, ports = ports })
            print(GREEN .. "Service spawned from image" .. RESET)
            print(json.encode(result))
        elseif action == "clone" then
            local result = Un.image_clone(target, { name = name })
            print(GREEN .. "Image cloned" .. RESET)
            print(json.encode(result))
        else
            print("Usage: lua un.lua image --list|--info ID|--delete ID|--lock ID|--unlock ID|--publish ID|--visibility ID MODE|--spawn ID|--clone ID")
        end
    elseif cmd == "-s" then
        -- Inline code execution
        local lang = args[2]
        local code = args[3]
        if not lang or not code then
            io.stderr:write(RED .. "Error: -s requires language and code" .. RESET .. "\n")
            os.exit(1)
        end
        local result = Un.execute(lang, code)
        if result then
            if result.stdout then print(result.stdout) end
            if result.stderr then io.stderr:write(result.stderr) end
            os.exit(result.exit_code or 0)
        else
            io.stderr:write(RED .. "Error: " .. Un.last_error() .. RESET .. "\n")
            os.exit(1)
        end
    elseif cmd == "--help" or cmd == "-h" then
        print("Usage: lua un.lua [options] <source_file>")
        print("       lua un.lua -s <language> '<code>'")
        print("       lua un.lua session [options]")
        print("       lua un.lua service [options]")
        print("       lua un.lua snapshot [options]")
        print("       lua un.lua image [options]")
        print("       lua un.lua languages [--json]")
        print("       lua un.lua key [--extend]")
    else
        -- Assume it's a file to execute
        run_file(cmd)
    end
end

return Un
