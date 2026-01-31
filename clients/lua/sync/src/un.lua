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
Un.VERSION = "4.2.51"

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

-- Image API functions
function Un.image_list(opts)
    opts = opts or {}
    return Un.api_request("GET", "/images", nil, opts)
end

function Un.image_get(image_id, opts)
    opts = opts or {}
    return Un.api_request("GET", "/images/" .. image_id, nil, opts)
end

function Un.image_delete(image_id, opts)
    opts = opts or {}
    return Un.api_request("DELETE", "/images/" .. image_id, nil, opts)
end

function Un.image_lock(image_id, opts)
    opts = opts or {}
    return Un.api_request("POST", "/images/" .. image_id .. "/lock", {}, opts)
end

function Un.image_unlock(image_id, opts)
    opts = opts or {}
    return Un.api_request("POST", "/images/" .. image_id .. "/unlock", {}, opts)
end

function Un.image_publish(source_id, source_type, name, opts)
    opts = opts or {}
    local body = {source_type = source_type, source_id = source_id}
    if name then body.name = name end
    return Un.api_request("POST", "/images/publish", body, opts)
end

function Un.image_visibility(image_id, visibility, opts)
    opts = opts or {}
    return Un.api_request("POST", "/images/" .. image_id .. "/visibility", {visibility = visibility}, opts)
end

function Un.image_spawn(image_id, name, ports, opts)
    opts = opts or {}
    local body = {}
    if name then body.name = name end
    if ports then body.ports = ports end
    return Un.api_request("POST", "/images/" .. image_id .. "/spawn", body, opts)
end

function Un.image_clone(image_id, name, opts)
    opts = opts or {}
    local body = {}
    if name then body.name = name end
    return Un.api_request("POST", "/images/" .. image_id .. "/clone", body, opts)
end

-- Service API functions
function Un.service_list(opts)
    opts = opts or {}
    return Un.api_request("GET", "/services", nil, opts)
end

function Un.service_get(service_id, opts)
    opts = opts or {}
    return Un.api_request("GET", "/services/" .. service_id, nil, opts)
end

function Un.service_set_unfreeze_on_demand(service_id, enabled, opts)
    opts = opts or {}
    return Un.api_request_patch("/services/" .. service_id, {unfreeze_on_demand = enabled}, opts)
end

function Un.api_request_patch(endpoint, body, opts)
    opts = opts or {}
    local pk, sk = Un.get_credentials(opts)

    local timestamp = tostring(os.time())
    local url = Un.API_BASE .. endpoint
    local body_str = body and json.encode(body) or "{}"
    local signature = Un.sign_request(sk, timestamp, "PATCH", endpoint, body_str)

    local headers = {
        ["Authorization"] = "Bearer " .. pk,
        ["X-Timestamp"] = timestamp,
        ["X-Signature"] = signature,
        ["Content-Type"] = "application/json"
    }

    local resp_body = {}
    local resp, status = https.request({
        url = url,
        method = "PATCH",
        headers = headers,
        source = body_str and ltn12.source.string(body_str),
        sink = ltn12.sink.table(resp_body)
    })

    if status ~= 200 then error("API error (" .. status .. ")") end
    return json.decode(table.concat(resp_body))
end

-- CLI
if arg and arg[1] then
    if arg[1] == "languages" then
        -- Languages command
        local json_output = arg[2] == "--json"
        local langs = Un.languages()

        if json_output then
            print(json.encode(langs))
        else
            for _, lang in ipairs(langs) do
                print(lang)
            end
        end
        os.exit(0)
    elseif arg[1] == "service" then
        -- Service command
        local i = 2
        local action = nil
        local service_id = nil
        local unfreeze_on_demand_value = nil

        while i <= #arg do
            if arg[i] == "--list" or arg[i] == "-l" then
                action = "list"
            elseif arg[i] == "--info" then
                action = "info"
                i = i + 1
                service_id = arg[i]
            elseif arg[i] == "--unfreeze-on-demand" then
                action = "unfreeze-on-demand"
                i = i + 1
                service_id = arg[i]
                if i + 1 <= #arg and (arg[i + 1] == "true" or arg[i + 1] == "false") then
                    i = i + 1
                    unfreeze_on_demand_value = arg[i] == "true"
                end
            end
            i = i + 1
        end

        if action == "list" then
            local result = Un.service_list()
            print(json.encode(result))
        elseif action == "info" then
            local result = Un.service_get(service_id)
            print(json.encode(result))
        elseif action == "unfreeze-on-demand" then
            if unfreeze_on_demand_value == nil then
                io.stderr:write("Error: --unfreeze-on-demand requires true or false\n")
                os.exit(1)
            end
            Un.service_set_unfreeze_on_demand(service_id, unfreeze_on_demand_value)
            print("Service unfreeze_on_demand set to " .. tostring(unfreeze_on_demand_value) .. ": " .. service_id)
        else
            io.stderr:write("Error: Use --list, --info ID, or --unfreeze-on-demand ID true|false\n")
            os.exit(1)
        end
        os.exit(0)
    elseif arg[1] == "image" then
        -- Image command
        local i = 2
        local action = nil
        local image_id = nil
        local name = nil
        local ports = nil
        local source_type = nil
        local visibility_mode = nil

        while i <= #arg do
            if arg[i] == "--list" or arg[i] == "-l" then
                action = "list"
            elseif arg[i] == "--info" then
                action = "info"
                i = i + 1
                image_id = arg[i]
            elseif arg[i] == "--delete" then
                action = "delete"
                i = i + 1
                image_id = arg[i]
            elseif arg[i] == "--lock" then
                action = "lock"
                i = i + 1
                image_id = arg[i]
            elseif arg[i] == "--unlock" then
                action = "unlock"
                i = i + 1
                image_id = arg[i]
            elseif arg[i] == "--publish" then
                action = "publish"
                i = i + 1
                image_id = arg[i]
            elseif arg[i] == "--source-type" then
                i = i + 1
                source_type = arg[i]
            elseif arg[i] == "--visibility" then
                action = "visibility"
                i = i + 1
                image_id = arg[i]
                if i + 1 <= #arg and not arg[i + 1]:match("^%-") then
                    i = i + 1
                    visibility_mode = arg[i]
                end
            elseif arg[i] == "--spawn" then
                action = "spawn"
                i = i + 1
                image_id = arg[i]
            elseif arg[i] == "--clone" then
                action = "clone"
                i = i + 1
                image_id = arg[i]
            elseif arg[i] == "--name" then
                i = i + 1
                name = arg[i]
            elseif arg[i] == "--ports" then
                i = i + 1
                ports = {}
                for p in arg[i]:gmatch("[^,]+") do
                    table.insert(ports, tonumber(p))
                end
            end
            i = i + 1
        end

        if action == "list" then
            local result = Un.image_list()
            print(json.encode(result))
        elseif action == "info" then
            local result = Un.image_get(image_id)
            print(json.encode(result))
        elseif action == "delete" then
            Un.image_delete(image_id)
            print("Image deleted: " .. image_id)
        elseif action == "lock" then
            Un.image_lock(image_id)
            print("Image locked: " .. image_id)
        elseif action == "unlock" then
            Un.image_unlock(image_id)
            print("Image unlocked: " .. image_id)
        elseif action == "publish" then
            if not source_type then
                io.stderr:write("Error: --source-type required (service or snapshot)\n")
                os.exit(1)
            end
            local result = Un.image_publish(image_id, source_type, name)
            print("Image published")
            print(json.encode(result))
        elseif action == "visibility" then
            if not visibility_mode then
                io.stderr:write("Error: --visibility requires MODE (private, unlisted, or public)\n")
                os.exit(1)
            end
            Un.image_visibility(image_id, visibility_mode)
            print("Image visibility set to " .. visibility_mode .. ": " .. image_id)
        elseif action == "spawn" then
            local result = Un.image_spawn(image_id, name, ports)
            print("Service spawned from image")
            print(json.encode(result))
        elseif action == "clone" then
            local result = Un.image_clone(image_id, name)
            print("Image cloned")
            print(json.encode(result))
        else
            io.stderr:write("Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID\n")
            os.exit(1)
        end
        os.exit(0)
    elseif arg[1] == "--help" or arg[1] == "-h" then
        print("Usage: lua un.lua [options] <source_file>")
        print("       lua un.lua languages [--json]")
        print("       lua un.lua service [options]")
        print("       lua un.lua image [options]")
        print("")
        print("Commands:")
        print("  languages [--json]  List available programming languages")
        print("  service [options]   Manage services")
        print("  image [options]     Manage images")
        print("")
        print("Languages options:")
        print("  --json              Output as JSON array")
        print("")
        print("Service options:")
        print("  --list              List all services")
        print("  --info ID           Get service details")
        print("  --unfreeze-on-demand ID true|false  Enable/disable auto-unfreeze on HTTP request")
        print("")
        print("Image options:")
        print("  --list              List all images")
        print("  --info ID           Get image details")
        print("  --delete ID         Delete an image")
        print("  --lock ID           Lock image to prevent deletion")
        print("  --unlock ID         Unlock image")
        print("  --publish ID        Publish image (requires --source-type)")
        print("  --source-type TYPE  Source type: service or snapshot")
        print("  --visibility ID MODE  Set visibility: private, unlisted, public")
        print("  --spawn ID          Spawn service from image")
        print("  --clone ID          Clone an image")
        print("  --name NAME         Name for spawned service or cloned image")
        print("  --ports PORTS       Ports for spawned service")
        os.exit(0)
    else
        local result = Un.run(arg[1])
        if result.stdout then print(result.stdout) end
        if result.stderr then io.stderr:write(result.stderr) end
        os.exit(result.exit_code or 0)
    end
end

return Un
