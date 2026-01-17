#!/usr/bin/env lua
-- PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
--
-- This is free public domain software for the public good of a permacomputer hosted
-- at permacomputer.com - an always-on computer by the people, for the people.
--
-- Learn more: https://www.permacomputer.com
--
-- Copyright 2025 TimeHexOn & foxhop & russell@unturf

local json = require("json")
local http = require("socket.http")
local https = require("ssl.https")
local ltn12 = require("ltn12")

local Un = {}
Un.API_BASE = "https://api.unsandbox.com"
Un.VERSION = "2.0.0"

-- Credential loading
function Un.load_accounts_csv(path)
    path = path or (os.getenv("HOME") .. "/.unsandbox/accounts.csv")
    local file = io.open(path, "r")
    if not file then return {} end

    local accounts = {}
    for line in file:lines() do
        line = line:match("^%s*(.-)%s*$")
        if line ~= "" then
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

    -- Tier 3: Home directory
    local accounts = Un.load_accounts_csv()
    if #accounts > 0 then return accounts[1][1], accounts[1][2] end

    -- Tier 4: Local directory
    accounts = Un.load_accounts_csv("./accounts.csv")
    if #accounts > 0 then return accounts[1][1], accounts[1][2] end

    error("No credentials found")
end

-- HMAC signature
function Un.sign_request(secret, timestamp, method, endpoint, body)
    local hmac = require("crypto").hmac
    local message = timestamp .. ":" .. method .. ":" .. endpoint .. ":" .. body
    return hmac.digest("sha256", message, secret, true):hex()
end

-- API request
function Un.api_request(method, endpoint, body, opts)
    opts = opts or {}
    local pk, sk = Un.get_credentials(opts)

    local timestamp = tostring(os.time())
    local url = Un.API_BASE .. endpoint
    local body_str = body and json.encode(body) or "{}"
    local signature = Un.sign_request(sk, timestamp, method, endpoint, body_str)

    local headers = {
        ["Authorization"] = "Bearer " .. pk,
        ["X-Timestamp"] = timestamp,
        ["X-Signature"] = signature,
        ["Content-Type"] = "application/json"
    }

    local resp_body = {}
    local resp, status = https.request({
        url = url,
        method = method,
        headers = headers,
        source = body_str and ltn12.source.string(body_str),
        sink = ltn12.sink.table(resp_body)
    })

    if status ~= 200 then error("API error (" .. status .. ")") end
    return json.decode(table.concat(resp_body))
end

-- Languages with cache
function Un.languages(opts)
    opts = opts or {}
    local cache_ttl = opts.cache_ttl or 3600
    local cache_path = os.getenv("HOME") .. "/.unsandbox/languages.json"

    local file = io.open(cache_path, "r")
    if file then
        local mtime = os.time() - (lfs.attributes(cache_path, "modification") or 0)
        if mtime < cache_ttl then
            local content = file:read("*a")
            file:close()
            return json.decode(content)
        end
        file:close()
    end

    local result = Un.api_request("GET", "/languages", nil, opts)
    local langs = result.languages or {}

    os.execute("mkdir -p " .. os.getenv("HOME") .. "/.unsandbox")
    file = io.open(cache_path, "w")
    file:write(json.encode(langs))
    file:close()

    return langs
end

-- Execute functions
function Un.execute(language, code, opts)
    opts = opts or {}
    local body = {
        language = language,
        code = code,
        network_mode = opts.network_mode or "zerotrust",
        ttl = opts.ttl or 60
    }
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

function Un.run(file, opts)
    local f = io.open(file, "r")
    local code = f:read("*a")
    f:close()
    return Un.execute(Un.detect_language(file), code, opts)
end

-- Job management
function Un.get_job(job_id, opts)
    opts = opts or {}
    return Un.api_request("GET", "/jobs/" .. job_id, nil, opts)
end

function Un.wait(job_id, timeout, opts)
    opts = opts or {}
    timeout = timeout or 3600
    local delays = {300, 450, 700, 900, 650, 1600, 2000}

    local start = os.time()
    for i = 0, 119 do
        local job = Un.get_job(job_id, opts)
        if job.status == "completed" then return job end
        if job.status == "failed" then error("Job failed") end

        if os.time() - start > timeout then error("Polling timeout") end

        local delay = delays[(i % 7) + 1] or 2000
        require("socket").sleep(delay / 1000)
    end

    error("Max polls exceeded")
end

-- Utilities
function Un.detect_language(filename)
    local ext = filename:match("%.([^%.]+)$")
    local map = {py="python", lua="lua", sh="bash", rb="ruby"}
    return map[ext] or error("Unknown file type")
end

-- CLI
if arg and arg[1] then
    local result = Un.run(arg[1])
    if result.stdout then print(result.stdout) end
    if result.stderr then io.stderr:write(result.stderr) end
    os.exit(result.exit_code or 0)
end

return Un
