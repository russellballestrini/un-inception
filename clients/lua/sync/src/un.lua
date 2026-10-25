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
local _api_url = os.getenv("UNSANDBOX_API_URL")
if not _api_url or _api_url == "" then _api_url = "https://api.unsandbox.com" end
Un.API_BASE = (_api_url:gsub("/+$", ""))
Un.PORTAL_BASE = "https://unsandbox.com"
Un.VERSION = "4.3.4"
Un.LAST_ERROR = ""
Un.ACCOUNT_INDEX = -1  -- -1 means not set; set to N to use accounts.csv row N

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

-- ============================================================================
-- SHA-256 / HMAC-SHA256, computed in-process (Lua 5.1 - 5.4)
-- ============================================================================

local MOD32 = 4294967296
local band, bxor, rshift
do
    -- Lua 5.3+: native integer bit operators (compiled at runtime so 5.1 still parses this file)
    local native = (loadstring or load)([[
        return function(a, b) return a & b end,
               function(a, b) return a ~ b end,
               function(a, n) return (a & 0xFFFFFFFF) >> n end
    ]])
    if native then
        band, bxor, rshift = native()
    else
        -- Lua 5.1 / 5.2 / LuaJIT: plain arithmetic, bit by bit
        band = function(a, b)
            local r, p = 0, 1
            for _ = 1, 32 do
                local x, y = a % 2, b % 2
                if x == 1 and y == 1 then r = r + p end
                a, b, p = (a - x) / 2, (b - y) / 2, p * 2
            end
            return r
        end
        bxor = function(a, b)
            local r, p = 0, 1
            for _ = 1, 32 do
                local x, y = a % 2, b % 2
                if x ~= y then r = r + p end
                a, b, p = (a - x) / 2, (b - y) / 2, p * 2
            end
            return r
        end
        rshift = function(a, n)
            return math.floor((a % MOD32) / 2 ^ n)
        end
    end
end

local function rrot(x, n)
    -- the two halves share no bits, so + acts as |
    return rshift(x, n) + (x * 2 ^ (32 - n)) % MOD32
end

local SHA256_K = {
    0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
    0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
    0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
    0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
    0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
    0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
    0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
    0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2,
}

-- Raw 32-byte SHA-256 digest of msg
local function sha256(msg)
    local H = { 0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab, 0x5be0cd19 }
    local len = #msg
    local bits, tail = len * 8, {}
    for i = 8, 1, -1 do
        tail[i] = string.char(bits % 256)
        bits = math.floor(bits / 256)
    end
    msg = msg .. "\128" .. string.rep("\0", (55 - len) % 64) .. table.concat(tail)

    for chunk = 1, #msg, 64 do
        local w = {}
        for i = 0, 15 do
            local b1, b2, b3, b4 = msg:byte(chunk + i * 4, chunk + i * 4 + 3)
            w[i] = ((b1 * 256 + b2) * 256 + b3) * 256 + b4
        end
        for i = 16, 63 do
            local x, y = w[i - 15], w[i - 2]
            local s0 = bxor(bxor(rrot(x, 7), rrot(x, 18)), rshift(x, 3))
            local s1 = bxor(bxor(rrot(y, 17), rrot(y, 19)), rshift(y, 10))
            w[i] = (w[i - 16] + s0 + w[i - 7] + s1) % MOD32
        end
        local a, b, c, d, e, f, g, h = H[1], H[2], H[3], H[4], H[5], H[6], H[7], H[8]
        for i = 0, 63 do
            local S1 = bxor(bxor(rrot(e, 6), rrot(e, 11)), rrot(e, 25))
            local ch = bxor(band(e, f), band(MOD32 - 1 - e, g))
            local t1 = (h + S1 + ch + SHA256_K[i + 1] + w[i]) % MOD32
            local S0 = bxor(bxor(rrot(a, 2), rrot(a, 13)), rrot(a, 22))
            local maj = bxor(bxor(band(a, b), band(a, c)), band(b, c))
            local t2 = (S0 + maj) % MOD32
            h, g, f, e, d, c, b, a = g, f, e, (d + t1) % MOD32, c, b, a, (t1 + t2) % MOD32
        end
        H[1], H[2], H[3], H[4] = (H[1] + a) % MOD32, (H[2] + b) % MOD32, (H[3] + c) % MOD32, (H[4] + d) % MOD32
        H[5], H[6], H[7], H[8] = (H[5] + e) % MOD32, (H[6] + f) % MOD32, (H[7] + g) % MOD32, (H[8] + h) % MOD32
    end

    local out = {}
    for i = 1, 8 do
        local v = H[i]
        out[i] = string.char(math.floor(v / 16777216) % 256, math.floor(v / 65536) % 256,
                             math.floor(v / 256) % 256, v % 256)
    end
    return table.concat(out)
end

local function to_hex(raw)
    return (raw:gsub(".", function(c) return string.format("%02x", c:byte()) end))
end

function Un.sha256_hex(message)
    return to_hex(sha256(message))
end

function Un.hmac_sign(secret, message)
    if not secret or not message then return nil end
    local key = secret
    if #key > 64 then key = sha256(key) end
    key = key .. string.rep("\0", 64 - #key)
    local ipad, opad = {}, {}
    for i = 1, 64 do
        local k = key:byte(i)
        ipad[i] = string.char(bxor(k, 0x36))
        opad[i] = string.char(bxor(k, 0x5c))
    end
    return to_hex(sha256(table.concat(opad) .. sha256(table.concat(ipad) .. message)))
end

-- Plain http:// for a local / staging UNSANDBOX_API_URL, https otherwise
local function transport_request(req)
    if req.url:match("^http://") then
        return http.request(req)
    end
    return https.request(req)
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
            -- public_key,secret_key[,label...]
            local pk, sk = line:match("^([^,]+),([^,]+)")
            if pk and sk then
                table.insert(accounts, {pk:match("^%s*(.-)%s*$"), sk:match("^%s*(.-)%s*$")})
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

    -- Tier 2: --account N flag → bypass env vars, load CSV row N directly
    local ai = opts.account_index
    if (ai == nil) and Un.ACCOUNT_INDEX >= 0 then ai = Un.ACCOUNT_INDEX end
    if ai and ai >= 0 then
        local row = ai + 1  -- Lua tables are 1-indexed
        local accounts = Un.load_accounts_csv()
        if #accounts >= row then return accounts[row][1], accounts[row][2] end
        accounts = Un.load_accounts_csv("./accounts.csv")
        if #accounts >= row then return accounts[row][1], accounts[row][2] end
        Un.set_error("Account index " .. ai .. " not found in accounts.csv")
        return nil, nil
    end

    -- Tier 3: Environment
    local pk = os.getenv("UNSANDBOX_PUBLIC_KEY")
    local sk = os.getenv("UNSANDBOX_SECRET_KEY")
    if pk and sk then return pk, sk end

    -- Legacy fallback
    if os.getenv("UNSANDBOX_API_KEY") then
        return os.getenv("UNSANDBOX_API_KEY"), ""
    end

    -- Tier 4: Home directory (row UNSANDBOX_ACCOUNT, default 0)
    local row = (tonumber(os.getenv("UNSANDBOX_ACCOUNT") or "") or 0) + 1
    local accounts = Un.load_accounts_csv()
    if #accounts >= row then return accounts[row][1], accounts[row][2] end

    -- Tier 5: Local directory
    accounts = Un.load_accounts_csv("./accounts.csv")
    if #accounts >= row then return accounts[row][1], accounts[row][2] end

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
        -- The server verifies against the request path without the query string
        -- (conn.request_path), so sign only the part before '?'.
        signature = Un.hmac_sign(sk, timestamp .. ":" .. method .. ":" .. endpoint:match("^[^?]*") .. ":" .. body_str)
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
    local resp, status = transport_request({
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
        -- The server verifies against the request path without the query string
        -- (conn.request_path), so sign only the part before '?'.
        signature = Un.hmac_sign(sk, timestamp .. ":" .. method .. ":" .. endpoint:match("^[^?]*") .. ":" .. body_str)
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
    local resp, status = transport_request({
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

-- POST /keys/validate; returns HTTP status (or nil) and the raw body.
function Un.validate_keys_raw(opts)
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
    local resp, status = transport_request({
        url = Un.API_BASE .. "/keys/validate",
        method = "POST",
        headers = headers,
        sink = ltn12.sink.table(resp_body)
    })

    return status, table.concat(resp_body)
end

function Un.validate_keys(opts)
    local status, response_text = Un.validate_keys_raw(opts)
    if status == 200 then
        local ok, result = pcall(json.decode, response_text)
        if ok then return result end
    end
    return nil
end

function Un.health_check(opts)
    local resp_body = {}
    local resp, status = transport_request({
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

local function shell_quote(s)
    return "'" .. s:gsub("'", "'\\''") .. "'"
end

-- Write content to path with mode 0600 from creation onward
local function write_private_file(path, content)
    os.execute("umask 077 && : > " .. shell_quote(path))
    local f = io.open(path, "w")
    if not f then return false end
    f:write(content)
    f:close()
    os.execute("chmod 600 " .. shell_quote(path))
    return true
end

local function stdin_is_tty()
    local r = os.execute("test -t 0")
    return r == true or r == 0
end

-- The accounts.csv the CLI's credentials came from, or nil (flags / env vars).
-- Mirrors Un.get_credentials' tiers.
local function credentials_csv_path()
    local home_csv = os.getenv("HOME") .. "/.unsandbox/accounts.csv"
    local row
    if Un.ACCOUNT_INDEX >= 0 then
        row = Un.ACCOUNT_INDEX + 1
    else
        if os.getenv("UNSANDBOX_PUBLIC_KEY") and os.getenv("UNSANDBOX_SECRET_KEY") then return nil end
        if os.getenv("UNSANDBOX_API_KEY") then return nil end
        row = (tonumber(os.getenv("UNSANDBOX_ACCOUNT") or "") or 0) + 1
    end
    for _, path in ipairs({ home_csv, "./accounts.csv" }) do
        if #Un.load_accounts_csv(path) >= row then return path end
    end
    return nil
end

-- Replace the secret on the line whose first field is public_key. Every other
-- line, field, comment and the order stay as they are. Temp file (0600) + rename.
local function rewrite_accounts_csv(path, public_key, new_secret)
    local f = io.open(path, "r")
    if not f then return false end
    local content = f:read("*a")
    f:close()

    local lines, changed = {}, false
    for line in (content .. "\n"):gmatch("(.-)\n") do
        local trimmed = line:match("^%s*(.-)%s*$")
        if not changed and trimmed ~= "" and not trimmed:match("^#") then
            local fields = {}
            for field in (line .. ","):gmatch("(.-),") do table.insert(fields, field) end
            if #fields >= 2 and fields[1]:match("^%s*(.-)%s*$") == public_key then
                fields[2] = new_secret
                line = table.concat(fields, ",")
                changed = true
            end
        end
        table.insert(lines, line)
    end
    if not changed then return false end

    local dir, base = path:match("^(.*)/([^/]+)$")
    if not dir then dir, base = ".", path end
    local tmp = dir .. "/." .. base .. "." .. tostring(os.time()) .. tostring(math.random(100000, 999999)) .. ".tmp"
    if not write_private_file(tmp, table.concat(lines, "\n")) then
        os.remove(tmp)
        return false
    end
    local ok = os.rename(tmp, path)
    if not ok then os.remove(tmp) end
    return ok and true or false
end

-- POST /keys/self/rotate-secret with body {}, optionally with sudo headers.
-- Returns status code, decoded body table.
local function rotate_post(pk, sk, otp, challenge)
    local endpoint = "/keys/self/rotate-secret"
    local body = "{}"
    local timestamp = tostring(os.time())
    local headers = {
        ["Authorization"] = "Bearer " .. pk,
        ["X-Timestamp"] = timestamp,
        ["X-Signature"] = Un.hmac_sign(sk, timestamp .. ":POST:" .. endpoint .. ":" .. body),
        ["Content-Type"] = "application/json",
        ["Content-Length"] = tostring(#body),
    }
    if otp then
        headers["X-Sudo-OTP"] = otp
        headers["X-Sudo-Challenge"] = challenge or ""
    end
    local resp_body = {}
    local _, status = transport_request({
        url = Un.API_BASE .. endpoint,
        method = "POST",
        headers = headers,
        source = ltn12.source.string(body),
        sink = ltn12.sink.table(resp_body)
    })
    local data = {}
    local ok, decoded = pcall(json.decode, table.concat(resp_body))
    if ok and type(decoded) == "table" then data = decoded end
    return tonumber(status) or 0, data
end

-- key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md)
local function cli_key_rotate(otp_arg)
    local pk, sk = Un.get_credentials()
    if not pk or not sk or sk == "" then
        io.stderr:write("Error: " .. Un.last_error() .. "\n")
        os.exit(3)
    end
    local dir = os.getenv("HOME") .. "/.unsandbox"
    local challenge_file = dir .. "/rotate-" .. Un.sha256_hex(pk):sub(1, 16) .. ".challenge"
    local status, data

    if otp_arg then
        local f = io.open(challenge_file, "r")
        if not f then
            io.stderr:write("Error: no rotation in progress; run: un key --rotate\n")
            os.exit(2)
        end
        local challenge = (f:read("*a") or ""):match("^%s*(.-)%s*$")
        f:close()
        status, data = rotate_post(pk, sk, otp_arg:match("^%s*(.-)%s*$"), challenge)
        os.remove(challenge_file)
    else
        status, data = rotate_post(pk, sk)
        if status == 428 then
            local challenge = tostring(data.challenge_id or "")
            if stdin_is_tty() then
                io.stderr:write("One-time code emailed to the key owner.\n")
                io.stderr:write("Code: ")
                io.stderr:flush()
                local otp = (io.read("*line") or ""):match("^%s*(.-)%s*$")
                if otp == "" then
                    io.stderr:write("Error: cancelled\n")
                    os.exit(1)
                end
                status, data = rotate_post(pk, sk, otp, challenge)
            else
                os.execute("mkdir -p -m 700 " .. shell_quote(dir))
                write_private_file(challenge_file, challenge)
                print("One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE")
                return
            end
        end
    end

    if status ~= 200 then
        if status == 403 and data.error == "email_required" then
            io.stderr:write("Error: this key has no email; add one or use POST /keys/recover\n")
            os.exit(3)
        end
        if status == 401 then
            io.stderr:write("Error: authentication failed\n")
            os.exit(3)
        end
        io.stderr:write("Error: " .. tostring(data.message or data.error or ("HTTP " .. tostring(status))) .. "\n")
        os.exit(4)
    end

    local new_secret = data.secret
    if type(new_secret) ~= "string" or new_secret == "" then
        io.stderr:write("Error: API returned no secret\n")
        os.exit(4)
    end

    local csv_path = credentials_csv_path()
    if csv_path and rewrite_accounts_csv(csv_path, pk, new_secret) then
        print("Secret rotated. Updated " .. csv_path .. " (public key " .. pk .. ").")
    else
        print("New secret (shown once, store it now): " .. new_secret)
        print("Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret.")
    end
end

-- CLI entry point
if arg and arg[0] then
    local args = arg
    local i = 1

    -- Pre-scan for --account N; strip it from args before dispatch
    local _filtered = {}
    local _j = 1
    while _j <= #args do
        if args[_j] == "--account" then
            _j = _j + 1
            Un.ACCOUNT_INDEX = tonumber(args[_j]) or -1
        else
            table.insert(_filtered, args[_j])
        end
        _j = _j + 1
    end
    for k = 1, #_filtered do args[k] = _filtered[k] end
    for k = #_filtered + 1, #args do args[k] = nil end

    if #args == 0 then
        print("Usage: lua un.lua [options] <source_file>")
        print("       lua un.lua -s <language> '<code>'")
        print("       lua un.lua session [options]")
        print("       lua un.lua service [options]")
        print("       lua un.lua snapshot [options]")
        print("       lua un.lua image [options]")
        print("       lua un.lua languages [--json]")
        print("       lua un.lua key [--extend]")
        print("       lua un.lua key --rotate [--otp CODE]")
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
        local rotate, otp = false, nil
        local k = 2
        while k <= #args do
            if args[k] == "--rotate" then
                rotate = true
            elseif args[k] == "--otp" then
                k = k + 1
                otp = args[k]
            end
            k = k + 1
        end
        if rotate then
            cli_key_rotate(otp)
            os.exit(0)
        end
        local pk = Un.get_credentials({})
        local status, text = Un.validate_keys_raw()
        if type(status) ~= "number" then
            io.stderr:write(RED .. "Error: request to " .. Un.API_BASE .. "/keys/validate failed" .. RESET .. "\n")
            os.exit(1)
        end
        local okj, result = pcall(json.decode, text or "")
        if not okj or type(result) ~= "table" then result = {} end
        local function str(v)
            if type(v) == "number" and v == math.floor(v) then return string.format("%d", v) end
            return tostring(v)
        end
        local state, reason
        if status == 200 then
            state = result.status or (result.valid and "active" or "invalid")
            reason = "status " .. str(state)
        elseif status == 401 then
            reason = result.error or "unauthorized"
            if reason == ":expired" or reason == "expired" then state = "expired"
            elseif reason == ":suspended" or reason == "suspended" then state = "suspended"
            else state = "invalid" end
        else
            local msg = result.error or result.message
            io.stderr:write(RED .. "Error: HTTP " .. status .. (msg and (" " .. str(msg)) or "") .. RESET .. "\n")
            os.exit(4)
        end
        if state == "expired" then
            print(RED .. "Expired" .. RESET)
            print("Public Key: " .. pk)
            print(YELLOW .. "To renew: Visit https://unsandbox.com/keys/extend" .. RESET)
            os.exit(3)
        elseif state == "suspended" then
            print(RED .. "Suspended" .. RESET)
            print("Public Key: " .. pk)
            os.exit(3)
        elseif state ~= "active" then
            print(RED .. "Invalid" .. RESET)
            print("Public Key: " .. pk)
            print("Reason: " .. str(reason))
            os.exit(3)
        end
        local expires, remaining = "N/A", "N/A"
        if type(result.valid_through) == "number" then
            local secs = math.floor(result.valid_through / 1000)
            expires = os.date("!%Y-%m-%dT%H:%M:%SZ", secs)
            local left = secs - os.time()
            if left <= 0 then
                remaining = "expired"
            else
                remaining = string.format("%dd %dh %dm", math.floor(left / 86400), math.floor(left % 86400 / 3600), math.floor(left % 3600 / 60))
            end
        end
        print(GREEN .. "Valid" .. RESET)
        print("Public Key: " .. pk)
        print("Tier: " .. (result.tier ~= nil and str(result.tier) or "N/A"))
        print("Status: " .. (result.status ~= nil and str(result.status) or "N/A"))
        print("Expires: " .. expires)
        print("Time Remaining: " .. remaining)
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
        print("       lua un.lua key --rotate [--otp CODE]")
    else
        -- Assume it's a file to execute
        run_file(cmd)
    end
end

return Un
