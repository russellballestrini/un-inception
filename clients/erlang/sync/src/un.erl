#!/usr/bin/env escript
%% PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
%%
%% This is free public domain software for the public good of a permacomputer hosted
%% at permacomputer.com - an always-on computer by the people, for the people. One
%% which is durable, easy to repair, and distributed like tap water for machine
%% learning intelligence.
%%
%% The permacomputer is community-owned infrastructure optimized around four values:
%%
%%   TRUTH    - First principles, math & science, open source code freely distributed
%%   FREEDOM  - Voluntary partnerships, freedom from tyranny & corporate control
%%   HARMONY  - Minimal waste, self-renewing systems with diverse thriving connections
%%   LOVE     - Be yourself without hurting others, cooperation through natural law
%%
%% This software contributes to that vision by enabling code execution across 42+
%% programming languages through a unified interface, accessible to all. Code is
%% seeds to sprout on any abandoned technology.
%%
%% Learn more: https://www.permacomputer.com
%%
%% Anyone is free to copy, modify, publish, use, compile, sell, or distribute this
%% software, either in source code form or as a compiled binary, for any purpose,
%% commercial or non-commercial, and by any means.
%%
%% NO WARRANTY. THE SOFTWARE IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND.
%%
%% That said, our permacomputer's digital membrane stratum continuously runs unit,
%% integration, and functional tests on all of it's own software - with our
%% permacomputer monitoring itself, repairing itself, with minimal human in the
%% loop guidance. Our agents do their best.
%%
%% Copyright 2025 TimeHexOn & foxhop & russell@unturf
%% https://www.timehexon.com
%% https://www.foxhop.net
%% https://www.unturf.com/software



%%% @doc unsandbox.com Erlang SDK
%%%
%%% Full API with execution, sessions, services, snapshots, and images.
%%%
%%% Library Usage:
%%% ```
%%%     %% Execute code synchronously
%%%     Result = un:execute("python", "print(42)"),
%%%     io:format("~s~n", [maps:get(stdout, Result)]).
%%%
%%%     %% List sessions
%%%     Sessions = un:session_list().
%%%
%%%     %% Create a service
%%%     ServiceId = un:service_create("myapp", #{ports => "8080"}).
%%% ```
%%%
%%% Authentication Priority:
%%%   1. Function arguments (PublicKey, SecretKey)
%%%   2. --account N -> accounts.csv row N (bypasses env vars)
%%%   3. Environment variables (UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY)
%%%   4. ~/.unsandbox/accounts.csv row 0 (or UNSANDBOX_ACCOUNT env)
%%%   5. ./accounts.csv row 0

%% Library API functions go unused when run as an escript CLI.
-compile([nowarn_unused_function]).

-define(API_BASE, api_base()).
-define(PORTAL_BASE, "https://unsandbox.com").
-define(VERSION, "4.2.0").
-define(LANGUAGES_CACHE_TTL, 3600).

%% ============================================================================
%% Utility Functions
%% ============================================================================

%% @doc Return the SDK version.
version() -> ?VERSION.

%% @doc Check API health.
health_check() ->
    Cmd = "curl -s -o /dev/null -w '%{http_code}' " ++ ?API_BASE ++ "/health",
    Result = os:cmd(Cmd),
    string:trim(Result) == "200".

%% @doc Generate HMAC-SHA256 signature for a message.
hmac_sign(SecretKey, Message) ->
    hmac_sha256(SecretKey, Message).

%% @doc Detect language from filename extension.
detect_language(Filename) ->
    Ext = filename:extension(Filename),
    ext_to_lang(Ext).

%% ============================================================================
%% Execution Functions (8)
%% ============================================================================

%% @doc Execute code synchronously.
execute(Language, Code) ->
    execute(Language, Code, #{}).

execute(Language, Code, Opts) ->
    Json = build_execute_json_full(Language, Code, Opts),
    Response = api_post("/execute", Json, Opts),
    parse_result(Response).

%% @doc Execute code asynchronously, returning a job ID.
execute_async(Language, Code) ->
    execute_async(Language, Code, #{}).

execute_async(Language, Code, Opts) ->
    Json = build_execute_json_full(Language, Code, Opts),
    Response = api_post("/execute/async", Json, Opts),
    extract_json_field(Response, "job_id").

%% @doc Wait for a job to complete and return the result.
wait_job(JobId) ->
    wait_job(JobId, #{}).

wait_job(JobId, Opts) ->
    MaxPolls = maps:get(max_polls, Opts, 100),
    PollDelays = [300, 450, 700, 900, 650, 1600, 2000],
    do_wait_job(JobId, PollDelays, 0, MaxPolls, Opts).

do_wait_job(JobId, _PollDelays, PollCount, MaxPolls, _Opts) when PollCount >= MaxPolls ->
    #{success => false, stdout => "", stderr => "Max polls exceeded", exit_code => 1, job_id => JobId};
do_wait_job(JobId, PollDelays, PollCount, MaxPolls, Opts) ->
    DelayIdx = min(PollCount, length(PollDelays) - 1),
    Delay = lists:nth(DelayIdx + 1, PollDelays),
    timer:sleep(Delay),
    Job = get_job(JobId, Opts),
    Status = maps:get(status, Job, "unknown"),
    case lists:member(Status, ["completed", "failed", "timeout", "cancelled"]) of
        true ->
            Response = api_get("/jobs/" ++ JobId, Opts),
            parse_result(Response);
        false ->
            do_wait_job(JobId, PollDelays, PollCount + 1, MaxPolls, Opts)
    end.

%% @doc Get job status and details.
get_job(JobId) ->
    get_job(JobId, #{}).

get_job(JobId, Opts) ->
    Response = api_get("/jobs/" ++ JobId, Opts),
    #{
        id => JobId,
        status => case extract_json_field(Response, "status") of "" -> "unknown"; S -> S end,
        language => extract_json_field(Response, "language"),
        created_at => extract_json_number(Response, "created_at"),
        completed_at => extract_json_number(Response, "completed_at")
    }.

%% @doc Cancel a running job.
cancel_job(JobId) ->
    cancel_job(JobId, #{}).

cancel_job(JobId, Opts) ->
    Response = api_delete("/jobs/" ++ JobId, Opts),
    not_contains_error(Response).

%% @doc List all active jobs.
list_jobs() ->
    list_jobs(#{}).

list_jobs(Opts) ->
    api_get("/jobs", Opts).

%% @doc Get list of supported languages.
get_languages() ->
    get_languages(#{}).

get_languages(Opts) ->
    case load_languages_cache() of
        undefined ->
            Response = api_get("/languages", Opts),
            Langs = extract_json_array(Response, "languages"),
            save_languages_cache(Langs),
            Langs;
        CachedLanguages ->
            CachedLanguages
    end.

%% ============================================================================
%% Session Functions (9)
%% ============================================================================

%% @doc List all sessions.
session_list() -> session_list(#{}).
session_list(Opts) -> api_get("/sessions", Opts).

%% @doc Get session details.
session_get(SessionId) -> session_get(SessionId, #{}).
session_get(SessionId, Opts) ->
    Response = api_get("/sessions/" ++ SessionId, Opts),
    #{
        id => SessionId,
        status => case extract_json_field(Response, "status") of "" -> "unknown"; S -> S end,
        container_name => extract_json_field(Response, "container_name"),
        network_mode => extract_json_field(Response, "network_mode"),
        vcpu => extract_json_number(Response, "vcpu"),
        created_at => extract_json_number(Response, "created_at")
    }.

%% @doc Create a new session.
session_create() -> session_create(#{}).
session_create(Opts) ->
    Shell = maps:get(shell, Opts, "bash"),
    Network = maps:get(network, Opts, undefined),
    Vcpu = maps:get(vcpu, Opts, undefined),
    NetworkJson = case Network of undefined -> ""; N -> ",\"network\":\"" ++ N ++ "\"" end,
    VcpuJson = case Vcpu of undefined -> ""; V -> ",\"vcpu\":" ++ integer_to_list(V) end,
    Json = "{\"shell\":\"" ++ Shell ++ "\"" ++ NetworkJson ++ VcpuJson ++ "}",
    Response = api_post("/sessions", Json, Opts),
    extract_json_field(Response, "id").

%% @doc Destroy a session.
session_destroy(SessionId) -> session_destroy(SessionId, #{}).
session_destroy(SessionId, Opts) ->
    Response = api_delete("/sessions/" ++ SessionId, Opts),
    not_contains_error(Response).

%% @doc Freeze a session.
session_freeze(SessionId) -> session_freeze(SessionId, #{}).
session_freeze(SessionId, Opts) ->
    Response = api_post("/sessions/" ++ SessionId ++ "/freeze", "{}", Opts),
    not_contains_error(Response).

%% @doc Unfreeze a session.
session_unfreeze(SessionId) -> session_unfreeze(SessionId, #{}).
session_unfreeze(SessionId, Opts) ->
    Response = api_post("/sessions/" ++ SessionId ++ "/unfreeze", "{}", Opts),
    not_contains_error(Response).

%% @doc Boost session resources.
session_boost(SessionId, Vcpu) -> session_boost(SessionId, Vcpu, #{}).
session_boost(SessionId, Vcpu, Opts) ->
    Json = "{\"vcpu\":" ++ integer_to_list(Vcpu) ++ "}",
    Response = api_patch("/sessions/" ++ SessionId, Json, Opts),
    not_contains_error(Response).

%% @doc Unboost session.
session_unboost(SessionId) -> session_unboost(SessionId, #{}).
session_unboost(SessionId, Opts) ->
    Response = api_patch("/sessions/" ++ SessionId, "{\"vcpu\":1}", Opts),
    not_contains_error(Response).

%% @doc Execute a command in a session.
session_execute(SessionId, Command) -> session_execute(SessionId, Command, #{}).
session_execute(SessionId, Command, Opts) ->
    Json = "{\"command\":\"" ++ escape_json(Command) ++ "\"}",
    Response = api_post("/sessions/" ++ SessionId ++ "/execute", Json, Opts),
    parse_result(Response).

%% ============================================================================
%% Service Functions (17)
%% ============================================================================

%% @doc List all services.
service_list() -> service_list(#{}).
service_list(Opts) -> api_get("/services", Opts).

%% @doc Get service details.
service_get(ServiceId) -> service_get(ServiceId, #{}).
service_get(ServiceId, Opts) ->
    Response = api_get("/services/" ++ ServiceId, Opts),
    #{
        id => ServiceId,
        name => extract_json_field(Response, "name"),
        status => case extract_json_field(Response, "status") of "" -> "unknown"; S -> S end,
        ports => extract_json_field(Response, "ports"),
        domains => extract_json_field(Response, "domains"),
        vcpu => extract_json_number(Response, "vcpu"),
        locked => extract_json_field(Response, "locked") == "true",
        unfreeze_on_demand => extract_json_field(Response, "unfreeze_on_demand") == "true",
        created_at => extract_json_number(Response, "created_at")
    }.

%% @doc Create a new service.
service_create(Name) -> service_create(Name, #{}).
service_create(Name, Opts) ->
    Ports = maps:get(ports, Opts, undefined),
    Bootstrap = maps:get(bootstrap, Opts, undefined),
    Network = maps:get(network, Opts, undefined),
    Vcpu = maps:get(vcpu, Opts, undefined),
    PortsJson = case Ports of undefined -> ""; P -> ",\"ports\":[" ++ P ++ "]" end,
    BootstrapJson = case Bootstrap of undefined -> ""; B -> ",\"bootstrap\":\"" ++ escape_json(B) ++ "\"" end,
    NetworkJson = case Network of undefined -> ""; N -> ",\"network\":\"" ++ N ++ "\"" end,
    VcpuJson = case Vcpu of undefined -> ""; V -> ",\"vcpu\":" ++ integer_to_list(V) end,
    Json = "{\"name\":\"" ++ escape_json(Name) ++ "\"" ++ PortsJson ++ BootstrapJson ++ NetworkJson ++ VcpuJson ++ "}",
    Response = api_post("/services", Json, Opts),
    extract_json_field(Response, "id").

%% @doc Destroy a service.
service_destroy(ServiceId) -> service_destroy(ServiceId, #{}).
service_destroy(ServiceId, Opts) ->
    Response = api_delete("/services/" ++ ServiceId, Opts),
    not_contains_error(Response).

%% @doc Freeze a service.
service_freeze(ServiceId) -> service_freeze(ServiceId, #{}).
service_freeze(ServiceId, Opts) ->
    Response = api_post("/services/" ++ ServiceId ++ "/freeze", "{}", Opts),
    not_contains_error(Response).

%% @doc Unfreeze a service.
service_unfreeze(ServiceId) -> service_unfreeze(ServiceId, #{}).
service_unfreeze(ServiceId, Opts) ->
    Response = api_post("/services/" ++ ServiceId ++ "/unfreeze", "{}", Opts),
    not_contains_error(Response).

%% @doc Lock a service.
service_lock(ServiceId) -> service_lock(ServiceId, #{}).
service_lock(ServiceId, Opts) ->
    Response = api_post("/services/" ++ ServiceId ++ "/lock", "{}", Opts),
    not_contains_error(Response).

%% @doc Unlock a service.
service_unlock(ServiceId) -> service_unlock(ServiceId, #{}).
service_unlock(ServiceId, Opts) ->
    Response = api_post("/services/" ++ ServiceId ++ "/unlock", "{}", Opts),
    not_contains_error(Response).

%% @doc Set unfreeze-on-demand for a service.
service_set_unfreeze_on_demand(ServiceId, Enabled) -> service_set_unfreeze_on_demand(ServiceId, Enabled, #{}).
service_set_unfreeze_on_demand(ServiceId, Enabled, Opts) ->
    EnabledStr = if Enabled -> "true"; true -> "false" end,
    Json = "{\"unfreeze_on_demand\":" ++ EnabledStr ++ "}",
    Response = api_patch("/services/" ++ ServiceId, Json, Opts),
    not_contains_error(Response).

%% @doc Redeploy a service.
service_redeploy(ServiceId) -> service_redeploy(ServiceId, undefined, #{}).
service_redeploy(ServiceId, Bootstrap) -> service_redeploy(ServiceId, Bootstrap, #{}).
service_redeploy(ServiceId, Bootstrap, Opts) ->
    BootstrapJson = case Bootstrap of undefined -> ""; B -> "\"bootstrap\":\"" ++ escape_json(B) ++ "\"" end,
    Json = "{" ++ BootstrapJson ++ "}",
    Response = api_post("/services/" ++ ServiceId ++ "/redeploy", Json, Opts),
    not_contains_error(Response).

%% @doc Get service logs.
service_logs(ServiceId) -> service_logs(ServiceId, #{}).
service_logs(ServiceId, Opts) ->
    AllLogs = maps:get(all_logs, Opts, false),
    Endpoint = if AllLogs -> "/services/" ++ ServiceId ++ "/logs?all=true"; true -> "/services/" ++ ServiceId ++ "/logs" end,
    api_get(Endpoint, Opts).

%% @doc Execute a command in a service.
service_execute(ServiceId, Command) -> service_execute(ServiceId, Command, #{}).
service_execute(ServiceId, Command, Opts) ->
    TimeoutMs = maps:get(timeout_ms, Opts, undefined),
    TimeoutJson = case TimeoutMs of undefined -> ""; T -> ",\"timeout_ms\":" ++ integer_to_list(T) end,
    Json = "{\"command\":\"" ++ escape_json(Command) ++ "\"" ++ TimeoutJson ++ "}",
    Response = api_post("/services/" ++ ServiceId ++ "/execute", Json, Opts),
    parse_result(Response).

%% @doc Get service environment vault.
service_env_get(ServiceId) -> service_env_get(ServiceId, #{}).
service_env_get(ServiceId, Opts) ->
    api_get("/services/" ++ ServiceId ++ "/env", Opts).

%% @doc Set service environment vault.
service_env_set(ServiceId, EnvContent) -> service_env_set(ServiceId, EnvContent, #{}).
service_env_set(ServiceId, EnvContent, Opts) ->
    api_put_text("/services/" ++ ServiceId ++ "/env", EnvContent, Opts).

%% @doc Delete service environment vault.
service_env_delete(ServiceId) -> service_env_delete(ServiceId, #{}).
service_env_delete(ServiceId, Opts) ->
    Response = api_delete("/services/" ++ ServiceId ++ "/env", Opts),
    not_contains_error(Response).

%% @doc Export service environment vault.
service_env_export(ServiceId) -> service_env_export(ServiceId, #{}).
service_env_export(ServiceId, Opts) ->
    Response = api_post("/services/" ++ ServiceId ++ "/env/export", "{}", Opts),
    extract_json_field(Response, "content").

%% @doc Resize a service.
service_resize(ServiceId, Vcpu) -> service_resize(ServiceId, Vcpu, #{}).
service_resize(ServiceId, Vcpu, Opts) ->
    Json = "{\"vcpu\":" ++ integer_to_list(Vcpu) ++ "}",
    Response = api_patch("/services/" ++ ServiceId, Json, Opts),
    not_contains_error(Response).

%% ============================================================================
%% Snapshot Functions (9)
%% ============================================================================

%% @doc List all snapshots.
snapshot_list() -> snapshot_list(#{}).
snapshot_list(Opts) -> api_get("/snapshots", Opts).

%% @doc Get snapshot details.
snapshot_get(SnapshotId) -> snapshot_get(SnapshotId, #{}).
snapshot_get(SnapshotId, Opts) ->
    Response = api_get("/snapshots/" ++ SnapshotId, Opts),
    #{
        id => SnapshotId,
        name => extract_json_field(Response, "name"),
        type => case extract_json_field(Response, "type") of "" -> "unknown"; T -> T end,
        source_id => extract_json_field(Response, "source_id"),
        hot => extract_json_field(Response, "hot") == "true",
        locked => extract_json_field(Response, "locked") == "true",
        created_at => extract_json_number(Response, "created_at"),
        size_bytes => extract_json_number(Response, "size_bytes")
    }.

%% @doc Create a snapshot of a session.
snapshot_session(SessionId) -> snapshot_session(SessionId, #{}).
snapshot_session(SessionId, Opts) ->
    Name = maps:get(name, Opts, undefined),
    Hot = maps:get(hot, Opts, false),
    NameJson = case Name of undefined -> ""; N -> "\"name\":\"" ++ escape_json(N) ++ "\"," end,
    HotJson = if Hot -> "\"hot\":true"; true -> "\"hot\":false" end,
    Json = "{" ++ NameJson ++ HotJson ++ "}",
    Response = api_post("/sessions/" ++ SessionId ++ "/snapshot", Json, Opts),
    extract_json_field(Response, "id").

%% @doc Create a snapshot of a service.
snapshot_service(ServiceId) -> snapshot_service(ServiceId, #{}).
snapshot_service(ServiceId, Opts) ->
    Name = maps:get(name, Opts, undefined),
    Hot = maps:get(hot, Opts, false),
    NameJson = case Name of undefined -> ""; N -> "\"name\":\"" ++ escape_json(N) ++ "\"," end,
    HotJson = if Hot -> "\"hot\":true"; true -> "\"hot\":false" end,
    Json = "{" ++ NameJson ++ HotJson ++ "}",
    Response = api_post("/services/" ++ ServiceId ++ "/snapshot", Json, Opts),
    extract_json_field(Response, "id").

%% @doc Restore from a snapshot.
snapshot_restore(SnapshotId) -> snapshot_restore(SnapshotId, #{}).
snapshot_restore(SnapshotId, Opts) ->
    Response = api_post("/snapshots/" ++ SnapshotId ++ "/restore", "{}", Opts),
    extract_json_field(Response, "id").

%% @doc Delete a snapshot.
snapshot_delete(SnapshotId) -> snapshot_delete(SnapshotId, #{}).
snapshot_delete(SnapshotId, Opts) ->
    Response = api_delete("/snapshots/" ++ SnapshotId, Opts),
    not_contains_error(Response).

%% @doc Lock a snapshot.
snapshot_lock(SnapshotId) -> snapshot_lock(SnapshotId, #{}).
snapshot_lock(SnapshotId, Opts) ->
    Response = api_post("/snapshots/" ++ SnapshotId ++ "/lock", "{}", Opts),
    not_contains_error(Response).

%% @doc Unlock a snapshot.
snapshot_unlock(SnapshotId) -> snapshot_unlock(SnapshotId, #{}).
snapshot_unlock(SnapshotId, Opts) ->
    Response = api_post("/snapshots/" ++ SnapshotId ++ "/unlock", "{}", Opts),
    not_contains_error(Response).

%% @doc Clone a snapshot to create a new session or service.
snapshot_clone(SnapshotId, Opts) ->
    CloneType = maps:get(type, Opts),
    Name = maps:get(name, Opts, undefined),
    Ports = maps:get(ports, Opts, undefined),
    Shell = maps:get(shell, Opts, undefined),
    TypeJson = "\"type\":\"" ++ CloneType ++ "\"",
    NameJson = case Name of undefined -> ""; N -> ",\"name\":\"" ++ escape_json(N) ++ "\"" end,
    PortsJson = case Ports of undefined -> ""; P -> ",\"ports\":[" ++ P ++ "]" end,
    ShellJson = case Shell of undefined -> ""; S -> ",\"shell\":\"" ++ S ++ "\"" end,
    Json = "{" ++ TypeJson ++ NameJson ++ PortsJson ++ ShellJson ++ "}",
    Response = api_post("/snapshots/" ++ SnapshotId ++ "/clone", Json, Opts),
    extract_json_field(Response, "id").

%% ============================================================================
%% Image Functions (13)
%% ============================================================================

%% @doc List images.
image_list() -> image_list(#{}).
image_list(Opts) ->
    Filter = maps:get(filter, Opts, undefined),
    Endpoint = case Filter of undefined -> "/images"; F -> "/images?filter=" ++ F end,
    api_get(Endpoint, Opts).

%% @doc Get image details.
image_get(ImageId) -> image_get(ImageId, #{}).
image_get(ImageId, Opts) ->
    Response = api_get("/images/" ++ ImageId, Opts),
    #{
        id => ImageId,
        name => extract_json_field(Response, "name"),
        description => extract_json_field(Response, "description"),
        visibility => case extract_json_field(Response, "visibility") of "" -> "private"; V -> V end,
        source_type => extract_json_field(Response, "source_type"),
        source_id => extract_json_field(Response, "source_id"),
        locked => extract_json_field(Response, "locked") == "true",
        created_at => extract_json_number(Response, "created_at"),
        size_bytes => extract_json_number(Response, "size_bytes")
    }.

%% @doc Publish an image.
image_publish(SourceType, SourceId) -> image_publish(SourceType, SourceId, #{}).
image_publish(SourceType, SourceId, Opts) ->
    Name = maps:get(name, Opts, undefined),
    Description = maps:get(description, Opts, undefined),
    NameJson = case Name of undefined -> ""; N -> ",\"name\":\"" ++ escape_json(N) ++ "\"" end,
    DescJson = case Description of undefined -> ""; D -> ",\"description\":\"" ++ escape_json(D) ++ "\"" end,
    Json = "{\"source_type\":\"" ++ SourceType ++ "\",\"source_id\":\"" ++ SourceId ++ "\"" ++ NameJson ++ DescJson ++ "}",
    Response = api_post("/images/publish", Json, Opts),
    extract_json_field(Response, "id").

%% @doc Delete an image.
image_delete(ImageId) -> image_delete(ImageId, #{}).
image_delete(ImageId, Opts) ->
    Response = api_delete("/images/" ++ ImageId, Opts),
    not_contains_error(Response).

%% @doc Lock an image.
image_lock(ImageId) -> image_lock(ImageId, #{}).
image_lock(ImageId, Opts) ->
    Response = api_post("/images/" ++ ImageId ++ "/lock", "{}", Opts),
    not_contains_error(Response).

%% @doc Unlock an image.
image_unlock(ImageId) -> image_unlock(ImageId, #{}).
image_unlock(ImageId, Opts) ->
    Response = api_post("/images/" ++ ImageId ++ "/unlock", "{}", Opts),
    not_contains_error(Response).

%% @doc Set image visibility.
image_set_visibility(ImageId, Visibility) -> image_set_visibility(ImageId, Visibility, #{}).
image_set_visibility(ImageId, Visibility, Opts) ->
    Json = "{\"visibility\":\"" ++ Visibility ++ "\"}",
    Response = api_post("/images/" ++ ImageId ++ "/visibility", Json, Opts),
    not_contains_error(Response).

%% @doc Grant access to an image.
image_grant_access(ImageId, TrustedApiKey) -> image_grant_access(ImageId, TrustedApiKey, #{}).
image_grant_access(ImageId, TrustedApiKey, Opts) ->
    Json = "{\"api_key\":\"" ++ TrustedApiKey ++ "\"}",
    Response = api_post("/images/" ++ ImageId ++ "/access/grant", Json, Opts),
    not_contains_error(Response).

%% @doc Revoke access to an image.
image_revoke_access(ImageId, TrustedApiKey) -> image_revoke_access(ImageId, TrustedApiKey, #{}).
image_revoke_access(ImageId, TrustedApiKey, Opts) ->
    Json = "{\"api_key\":\"" ++ TrustedApiKey ++ "\"}",
    Response = api_post("/images/" ++ ImageId ++ "/access/revoke", Json, Opts),
    not_contains_error(Response).

%% @doc List trusted API keys for an image.
image_list_trusted(ImageId) -> image_list_trusted(ImageId, #{}).
image_list_trusted(ImageId, Opts) ->
    Response = api_get("/images/" ++ ImageId ++ "/access", Opts),
    extract_json_array(Response, "trusted_keys").

%% @doc Transfer image ownership.
image_transfer(ImageId, ToApiKey) -> image_transfer(ImageId, ToApiKey, #{}).
image_transfer(ImageId, ToApiKey, Opts) ->
    Json = "{\"to_api_key\":\"" ++ ToApiKey ++ "\"}",
    Response = api_post("/images/" ++ ImageId ++ "/transfer", Json, Opts),
    not_contains_error(Response).

%% @doc Spawn a service from an image.
image_spawn(ImageId) -> image_spawn(ImageId, #{}).
image_spawn(ImageId, Opts) ->
    Name = maps:get(name, Opts, undefined),
    Ports = maps:get(ports, Opts, undefined),
    Bootstrap = maps:get(bootstrap, Opts, undefined),
    Network = maps:get(network, Opts, undefined),
    NameJson = case Name of undefined -> ""; N -> "\"name\":\"" ++ escape_json(N) ++ "\"" end,
    PortsJson = case Ports of undefined -> ""; P -> (if Name =/= undefined -> ","; true -> "" end) ++ "\"ports\":[" ++ P ++ "]" end,
    BootstrapJson = case Bootstrap of undefined -> ""; B -> ",\"bootstrap\":\"" ++ escape_json(B) ++ "\"" end,
    NetworkJson = case Network of undefined -> ""; Nn -> ",\"network\":\"" ++ Nn ++ "\"" end,
    Json = "{" ++ NameJson ++ PortsJson ++ BootstrapJson ++ NetworkJson ++ "}",
    Response = api_post("/images/" ++ ImageId ++ "/spawn", Json, Opts),
    extract_json_field(Response, "id").

%% @doc Clone an image.
image_clone(ImageId) -> image_clone(ImageId, #{}).
image_clone(ImageId, Opts) ->
    Name = maps:get(name, Opts, undefined),
    Description = maps:get(description, Opts, undefined),
    NameJson = case Name of undefined -> ""; N -> "\"name\":\"" ++ escape_json(N) ++ "\"" end,
    DescJson = case Description of undefined -> ""; D -> (if Name =/= undefined -> ","; true -> "" end) ++ "\"description\":\"" ++ escape_json(D) ++ "\"" end,
    Json = "{" ++ NameJson ++ DescJson ++ "}",
    Response = api_post("/images/" ++ ImageId ++ "/clone", Json, Opts),
    extract_json_field(Response, "id").

%% ============================================================================
%% PaaS Logs Functions (2)
%% ============================================================================

%% @doc Fetch batch logs from portal.
logs_fetch() -> logs_fetch(#{}).
logs_fetch(Opts) ->
    Source = maps:get(source, Opts, "all"),
    Lines = maps:get(lines, Opts, 100),
    Since = maps:get(since, Opts, "1h"),
    Grep = maps:get(grep, Opts, undefined),
    GrepParam = case Grep of undefined -> ""; G -> "&grep=" ++ uri_string:quote(G) end,
    api_get("/logs?source=" ++ Source ++ "&lines=" ++ integer_to_list(Lines) ++ "&since=" ++ Since ++ GrepParam, Opts).

%% @doc Stream logs (simplified polling implementation).
logs_stream(Callback) -> logs_stream(Callback, #{}).
logs_stream(Callback, Opts) ->
    Source = maps:get(source, Opts, "all"),
    Grep = maps:get(grep, Opts, undefined),
    Interval = maps:get(interval, Opts, 5000),
    GrepParam = case Grep of undefined -> ""; G -> "&grep=" ++ uri_string:quote(G) end,
    logs_stream_loop(Source, GrepParam, Callback, Interval, Opts).

logs_stream_loop(Source, GrepParam, Callback, Interval, Opts) ->
    Response = api_get("/logs?source=" ++ Source ++ "&lines=50&since=10s" ++ GrepParam, Opts),
    Callback(Source, Response),
    timer:sleep(Interval),
    logs_stream_loop(Source, GrepParam, Callback, Interval, Opts).

%% ============================================================================
%% Key Validation (1)
%% ============================================================================

%% @doc Validate API keys.
validate_keys() -> validate_keys(#{}).
validate_keys(Opts) ->
    Response = api_post("/keys/validate", "{}", Opts),
    #{
        valid => extract_json_field(Response, "status") == "valid",
        tier => extract_json_field(Response, "tier"),
        rate_limit_per_minute => extract_json_number(Response, "rate_per_minute"),
        concurrency_limit => extract_json_number(Response, "concurrency"),
        expires_at => extract_json_number(Response, "expires_at")
    }.

%% ============================================================================
%% Private API Functions
%% ============================================================================

api_get(Endpoint, Opts) ->
    Result = signed_curl(get_api_keys_from_opts(Opts), "GET", Endpoint, undefined, #{}),
    check_clock_drift_error(Result),
    Result.

api_post(Endpoint, Json, Opts) ->
    Result = signed_curl(get_api_keys_from_opts(Opts), "POST", Endpoint, Json, #{}),
    check_clock_drift_error(Result),
    Result.

api_delete(Endpoint, Opts) ->
    Result = signed_curl(get_api_keys_from_opts(Opts), "DELETE", Endpoint, undefined, #{}),
    check_clock_drift_error(Result),
    Result.

api_patch(Endpoint, Json, Opts) ->
    Result = signed_curl(get_api_keys_from_opts(Opts), "PATCH", Endpoint, Json, #{}),
    check_clock_drift_error(Result),
    Result.

api_put_text(Endpoint, Body, Opts) ->
    Result = signed_curl(get_api_keys_from_opts(Opts), "PUT", Endpoint, Body,
                         #{content_type => "text/plain", curl_args => " -o /dev/null -w '%{http_code}'"}),
    case string:to_integer(string:trim(Result)) of
        {StatusCode, _} when is_integer(StatusCode) -> StatusCode >= 200 andalso StatusCode < 300;
        _ -> false
    end.

%% Generic signed request used by the image CLI commands.
api_request(Endpoint, Method, Body, PublicKey, SecretKey) ->
    Opts = #{public_key => PublicKey, secret_key => SecretKey},
    case Method of
        "GET" -> api_get(Endpoint, Opts);
        "POST" -> api_post(Endpoint, Body, Opts);
        "DELETE" -> api_delete(Endpoint, Opts);
        "PATCH" -> api_patch(Endpoint, Body, Opts)
    end.

get_api_keys_from_opts(Opts) ->
    case {maps:get(public_key, Opts, undefined), maps:get(secret_key, Opts, undefined)} of
        {Pk, Sk} when Pk =/= undefined, Sk =/= undefined -> {Pk, Sk};
        _ -> get_api_keys()
    end.

build_execute_json_full(Language, Code, Opts) ->
    Network = maps:get(network, Opts, undefined),
    Vcpu = maps:get(vcpu, Opts, undefined),
    Ttl = maps:get(ttl, Opts, undefined),
    ReturnArtifacts = maps:get(return_artifacts, Opts, false),
    NetworkJson = case Network of undefined -> ""; N -> ",\"network\":\"" ++ N ++ "\"" end,
    VcpuJson = case Vcpu of undefined -> ""; V -> ",\"vcpu\":" ++ integer_to_list(V) end,
    TtlJson = case Ttl of undefined -> ""; T -> ",\"ttl\":" ++ integer_to_list(T) end,
    ArtifactsJson = if ReturnArtifacts -> ",\"return_artifacts\":true"; true -> "" end,
    "{\"language\":\"" ++ Language ++ "\",\"code\":\"" ++ escape_json(Code) ++ "\"" ++ NetworkJson ++ VcpuJson ++ TtlJson ++ ArtifactsJson ++ "}".

parse_result(Response) ->
    ExitCode = case extract_json_number(Response, "exit_code") of 0 -> 0; N when is_integer(N) -> N; _ -> 0 end,
    #{
        success => ExitCode == 0,
        stdout => case extract_json_field(Response, "stdout") of "" -> ""; S -> S end,
        stderr => case extract_json_field(Response, "stderr") of "" -> ""; S -> S end,
        exit_code => ExitCode,
        job_id => extract_json_field(Response, "job_id"),
        language => extract_json_field(Response, "language"),
        execution_time => undefined
    }.

not_contains_error(Response) ->
    string:str(Response, "\"error\"") == 0.

%% ============================================================================
%% CLI Entry Point
%% ============================================================================

main(RawArgs) ->
    %% Strip --account N from args and store index in process dict before dispatch
    {AccountIndex, Args} = extract_account_arg(RawArgs, undefined, []),
    case AccountIndex of
        undefined -> ok;
        N -> erlang:put(account_index, N)
    end,
    dispatch(Args).

dispatch([]) ->
    io:format("Usage: un.erl [--account N] [options] <source_file>~n"),
    io:format("       un.erl [--account N] session [options]~n"),
    io:format("       un.erl [--account N] service [options]~n"),
    io:format("       un.erl [--account N] snapshot [options]~n"),
    io:format("       un.erl [--account N] image [options]~n"),
    io:format("       un.erl [--account N] key [options]~n"),
    io:format("       un.erl languages [--json]~n"),
    io:format("~nGlobal options:~n"),
    io:format("  --account N    Use accounts.csv row N (bypasses env vars)~n"),
    halt(1);

dispatch(["session" | Rest]) ->
    session_command(Rest);

dispatch(["service" | Rest]) ->
    service_command(Rest);

dispatch(["snapshot" | Rest]) ->
    snapshot_command(Rest);

dispatch(["image" | Rest]) ->
    image_command(Rest);

dispatch(["key" | Rest]) ->
    key_command(Rest);

dispatch(["languages" | Rest]) ->
    languages_command(Rest);

dispatch(Args) ->
    execute_command(Args).

%% Strip --account N from argument list, return {Index | undefined, RestArgs}
extract_account_arg([], Acc, RestAcc) ->
    {Acc, lists:reverse(RestAcc)};
extract_account_arg(["--account", NStr | Rest], _Acc, RestAcc) ->
    N = try list_to_integer(NStr) catch _:_ ->
        io:format("Error: --account requires an integer argument~n"),
        halt(1)
    end,
    extract_account_arg(Rest, N, RestAcc);
extract_account_arg([Arg | Rest], Acc, RestAcc) ->
    extract_account_arg(Rest, Acc, [Arg | RestAcc]).

%% Execute command
execute_command(Args) ->
    {File, _Opts} = parse_exec_args(Args, #{file => undefined}),
    case File of
        undefined ->
            io:format("Error: No source file specified~n"),
            halt(1);
        _ ->
            ApiKey = get_api_key(),
            Ext = filename:extension(File),
            case ext_to_lang(Ext) of
                {ok, Language} ->
                    case file:read_file(File) of
                        {ok, CodeBin} ->
                            Code = binary_to_list(CodeBin),
                            Json = build_json(Language, Code),
                            TmpFile = write_temp_file(Json),
                            Response = curl_post(ApiKey, "/execute", TmpFile),
                            file:delete(TmpFile),
                            io:format("~s~n", [Response]);
                        {error, Reason} ->
                            io:format("Error reading file: ~p~n", [Reason]),
                            halt(1)
                    end;
                {error, Ext} ->
                    io:format("Error: Unknown extension: ~s~n", [Ext]),
                    halt(1)
            end
    end.

%% Session command
session_command(["--list" | _]) ->
    ApiKey = get_api_key(),
    Response = curl_get(ApiKey, "/sessions"),
    io:format("~s~n", [Response]);

session_command(["--kill", SessionId | _]) ->
    ApiKey = get_api_key(),
    _ = curl_delete(ApiKey, "/sessions/" ++ SessionId),
    io:format("\033[32mSession terminated: ~s\033[0m~n", [SessionId]);

%% Session snapshot commands
session_command(["--snapshot", SessionId | Rest]) ->
    ApiKey = get_api_key(),
    Name = get_snapshot_name(Rest),
    Hot = has_hot_flag(Rest),
    Json = build_snapshot_json(Name, Hot),
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/sessions/" ++ SessionId ++ "/snapshot", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mSnapshot created\033[0m~n"),
    io:format("~s~n", [Response]);

session_command(["--restore", SnapshotId | _Rest]) ->
    % --restore takes snapshot ID directly, calls /snapshots/:id/restore
    ApiKey = get_api_key(),
    TmpFile = write_temp_file("{}"),
    Response = curl_post(ApiKey, "/snapshots/" ++ SnapshotId ++ "/restore", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mSession restored from snapshot\033[0m~n"),
    io:format("~s~n", [Response]);

session_command(Args) ->
    validate_session_args(Args),
    ApiKey = get_api_key(),
    Shell = get_shell_opt(Args, "bash"),
    InputFiles = get_input_files(Args),
    InputFilesJson = build_input_files_json(InputFiles),
    Json = "{\"shell\":\"" ++ Shell ++ "\"" ++ InputFilesJson ++ "}",
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/sessions", TmpFile),
    file:delete(TmpFile),
    io:format("\033[33mSession created (WebSocket required)\033[0m~n"),
    io:format("~s~n", [Response]).

validate_session_args([]) -> ok;
validate_session_args(["--shell", _ | Rest]) -> validate_session_args(Rest);
validate_session_args(["-s", _ | Rest]) -> validate_session_args(Rest);
validate_session_args(["-f", _ | Rest]) -> validate_session_args(Rest);
validate_session_args(["-n", _ | Rest]) -> validate_session_args(Rest);
validate_session_args(["-v", _ | Rest]) -> validate_session_args(Rest);
validate_session_args(["--snapshot", _ | Rest]) -> validate_session_args(Rest);
validate_session_args(["--restore", _ | Rest]) -> validate_session_args(Rest);
validate_session_args(["--from", _ | Rest]) -> validate_session_args(Rest);
validate_session_args(["--snapshot-name", _ | Rest]) -> validate_session_args(Rest);
validate_session_args(["--hot" | Rest]) -> validate_session_args(Rest);
validate_session_args([Arg | _]) ->
    case Arg of
        [$- | _] ->
            io:format(standard_error, "Unknown option: ~s~n", [Arg]),
            io:format(standard_error, "Usage: un.erl session [options]~n", []),
            halt(1);
        _ ->
            validate_session_args([])
    end.

get_snapshot_name([]) -> undefined;
get_snapshot_name(["--snapshot-name", Name | _]) -> Name;
get_snapshot_name([_ | Rest]) -> get_snapshot_name(Rest).

get_from_snapshot([]) -> undefined;
get_from_snapshot(["--from", SnapshotId | _]) -> SnapshotId;
get_from_snapshot([_ | Rest]) -> get_from_snapshot(Rest).

has_hot_flag([]) -> false;
has_hot_flag(["--hot" | _]) -> true;
has_hot_flag([_ | Rest]) -> has_hot_flag(Rest).

build_snapshot_json(undefined, false) -> "{}";
build_snapshot_json(undefined, true) -> "{\"hot\":true}";
build_snapshot_json(Name, false) -> "{\"name\":\"" ++ escape_json(Name) ++ "\"}";
build_snapshot_json(Name, true) -> "{\"name\":\"" ++ escape_json(Name) ++ "\",\"hot\":true}".

%% Service command
service_command(["--list" | _]) ->
    ApiKey = get_api_key(),
    Response = curl_get(ApiKey, "/services"),
    io:format("~s~n", [Response]);

service_command(["--info", ServiceId | _]) ->
    ApiKey = get_api_key(),
    Response = curl_get(ApiKey, "/services/" ++ ServiceId),
    io:format("~s~n", [Response]);

service_command(["--logs", ServiceId | _]) ->
    ApiKey = get_api_key(),
    Response = curl_get(ApiKey, "/services/" ++ ServiceId ++ "/logs"),
    io:format("~s~n", [Response]);

service_command(["--freeze", ServiceId | _]) ->
    ApiKey = get_api_key(),
    TmpFile = write_temp_file("{}"),
    _ = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/freeze", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mService frozen: ~s\033[0m~n", [ServiceId]);

service_command(["--unfreeze", ServiceId | _]) ->
    ApiKey = get_api_key(),
    TmpFile = write_temp_file("{}"),
    _ = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/unfreeze", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mService unfreezing: ~s\033[0m~n", [ServiceId]);

service_command(["--destroy", ServiceId | _]) ->
    ApiKey = get_api_key(),
    case curl_delete_with_sudo(ApiKey, "/services/" ++ ServiceId) of
        {ok, _, _} ->
            io:format("\033[32mService destroyed: ~s\033[0m~n", [ServiceId]);
        {ok, _} ->
            io:format("\033[32mService destroyed: ~s\033[0m~n", [ServiceId]);
        {error, cancelled} ->
            halt(1);
        {error, Msg} ->
            io:format(standard_error, "\033[31mError: ~s\033[0m~n", [Msg]),
            halt(1)
    end;

service_command(["--resize", ServiceId, "--vcpu", VcpuStr | _]) ->
    cli_service_resize(ServiceId, VcpuStr);

service_command(["--resize", ServiceId, "-v", VcpuStr | _]) ->
    cli_service_resize(ServiceId, VcpuStr);

service_command(["--set-unfreeze-on-demand", ServiceId, EnabledStr | _]) ->
    cli_service_set_unfreeze_on_demand(ServiceId, EnabledStr);

service_command(["--execute", ServiceId, "--command", Command | _]) ->
    ApiKey = get_api_key(),
    Json = "{\"command\":\"" ++ escape_json(Command) ++ "\"}",
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/execute", TmpFile),
    file:delete(TmpFile),
    case extract_json_field(Response, "stdout") of
        "" -> ok;
        Stdout -> io:format("\033[34m~s\033[0m", [Stdout])
    end;

service_command(["--dump-bootstrap", ServiceId, File | _]) ->
    ApiKey = get_api_key(),
    io:format(standard_error, "Fetching bootstrap script from ~s...~n", [ServiceId]),
    Json = "{\"command\":\"cat /tmp/bootstrap.sh\"}",
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/execute", TmpFile),
    file:delete(TmpFile),
    case extract_json_field(Response, "stdout") of
        "" ->
            io:format(standard_error, "\033[31mError: Failed to fetch bootstrap (service not running or no bootstrap file)\033[0m~n", []),
            halt(1);
        Script ->
            file:write_file(File, Script),
            file:change_mode(File, 8#755),
            io:format("Bootstrap saved to ~s~n", [File])
    end;

service_command(["--dump-bootstrap", ServiceId | _]) ->
    ApiKey = get_api_key(),
    io:format(standard_error, "Fetching bootstrap script from ~s...~n", [ServiceId]),
    Json = "{\"command\":\"cat /tmp/bootstrap.sh\"}",
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/execute", TmpFile),
    file:delete(TmpFile),
    case extract_json_field(Response, "stdout") of
        "" ->
            io:format(standard_error, "\033[31mError: Failed to fetch bootstrap (service not running or no bootstrap file)\033[0m~n", []),
            halt(1);
        Script ->
            io:format("~s", [Script])
    end;

%% Service snapshot commands
service_command(["--snapshot", ServiceId | Rest]) ->
    ApiKey = get_api_key(),
    Name = get_snapshot_name(Rest),
    Hot = has_hot_flag(Rest),
    Json = build_snapshot_json(Name, Hot),
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/services/" ++ ServiceId ++ "/snapshot", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mSnapshot created\033[0m~n"),
    io:format("~s~n", [Response]);

service_command(["--restore", SnapshotId | _Rest]) ->
    % --restore takes snapshot ID directly, calls /snapshots/:id/restore
    ApiKey = get_api_key(),
    TmpFile = write_temp_file("{}"),
    Response = curl_post(ApiKey, "/snapshots/" ++ SnapshotId ++ "/restore", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mService restored from snapshot\033[0m~n"),
    io:format("~s~n", [Response]);

%% Service env vault subcommand: service env <action> <id> [options]
service_command(["env", "status", ServiceId | _]) ->
    service_env_status(ServiceId);

service_command(["env", "set", ServiceId | Rest]) ->
    EnvVars = get_env_vars(Rest),
    EnvFile = get_env_file(Rest),
    Content = build_env_content(EnvVars, EnvFile),
    case Content of
        "" ->
            io:format(standard_error, "Error: No environment variables specified. Use -e KEY=VALUE or --env-file FILE~n", []),
            halt(1);
        _ ->
            cli_service_env_set(ServiceId, Content)
    end;

service_command(["env", "export", ServiceId | _]) ->
    cli_service_env_export(ServiceId);

service_command(["env", "delete", ServiceId | _]) ->
    cli_service_env_delete(ServiceId);

service_command(["env" | _]) ->
    io:format(standard_error, "Usage: un.erl service env <status|set|export|delete> <service_id> [options]~n", []),
    halt(1);

service_command(Args) ->
    case get_service_name(Args) of
        undefined ->
            io:format("Error: --name required to create service~n"),
            halt(1);
        Name ->
            ApiKey = get_api_key(),
            Ports = get_service_ports(Args),
            Bootstrap = get_service_bootstrap(Args),
            BootstrapFile = get_service_bootstrap_file(Args),
            Type = get_service_type(Args),
            InputFiles = get_input_files(Args),
            EnvVars = get_env_vars(Args),
            EnvFile = get_env_file(Args),
            PortsJson = case Ports of
                undefined -> "";
                P -> ",\"ports\":[" ++ P ++ "]"
            end,
            BootstrapJson = case Bootstrap of
                undefined -> "";
                B -> ",\"bootstrap\":\"" ++ escape_json(B) ++ "\""
            end,
            BootstrapContentJson = case BootstrapFile of
                undefined -> "";
                BF ->
                    case file:read_file(BF) of
                        {ok, ContentBin} ->
                            Content = binary_to_list(ContentBin),
                            ",\"bootstrap_content\":\"" ++ escape_json(Content) ++ "\"";
                        {error, _} ->
                            io:format(standard_error, "\033[31mError: Bootstrap file not found: ~s\033[0m~n", [BF]),
                            halt(1)
                    end
            end,
            TypeJson = case Type of
                undefined -> "";
                T -> ",\"service_type\":\"" ++ T ++ "\""
            end,
            InputFilesJson = build_input_files_json(InputFiles),
            Json = "{\"name\":\"" ++ Name ++ "\"" ++ PortsJson ++ BootstrapJson ++ BootstrapContentJson ++ TypeJson ++ InputFilesJson ++ "}",
            TmpFile = write_temp_file(Json),
            Response = curl_post(ApiKey, "/services", TmpFile),
            file:delete(TmpFile),
            io:format("\033[32mService created\033[0m~n"),
            io:format("~s~n", [Response]),
            %% Auto-vault: set env vars if provided
            EnvContent = build_env_content(EnvVars, EnvFile),
            case EnvContent of
                "" -> ok;
                _ ->
                    ServiceId = extract_json_field(Response, "id"),
                    case ServiceId of
                        "" -> ok;
                        _ ->
                            io:format("Setting vault for ~s...~n", [ServiceId]),
                            cli_service_env_set(ServiceId, EnvContent)
                    end
            end
    end.

%% Snapshot command
snapshot_command(["--list" | _]) ->
    snapshot_command(["-l"]);
snapshot_command(["-l" | _]) ->
    ApiKey = get_api_key(),
    Response = curl_get(ApiKey, "/snapshots"),
    io:format("~s~n", [Response]);

snapshot_command(["--info", SnapshotId | _]) ->
    ApiKey = get_api_key(),
    Response = curl_get(ApiKey, "/snapshots/" ++ SnapshotId),
    io:format("~s~n", [Response]);

snapshot_command(["--delete", SnapshotId | _]) ->
    ApiKey = get_api_key(),
    case curl_delete_with_sudo(ApiKey, "/snapshots/" ++ SnapshotId) of
        {ok, _, _} ->
            io:format("\033[32mSnapshot deleted: ~s\033[0m~n", [SnapshotId]);
        {ok, _} ->
            io:format("\033[32mSnapshot deleted: ~s\033[0m~n", [SnapshotId]);
        {error, cancelled} ->
            halt(1);
        {error, Msg} ->
            io:format(standard_error, "\033[31mError: ~s\033[0m~n", [Msg]),
            halt(1)
    end;

snapshot_command(["--clone", SnapshotId | Rest]) ->
    ApiKey = get_api_key(),
    Type = get_clone_type(Rest),
    Name = get_clone_name(Rest),
    Shell = get_clone_shell(Rest),
    Ports = get_clone_ports(Rest),
    Json = build_clone_json(Type, Name, Shell, Ports),
    TmpFile = write_temp_file(Json),
    Response = curl_post(ApiKey, "/snapshots/" ++ SnapshotId ++ "/clone", TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mCreated from snapshot\033[0m~n"),
    io:format("~s~n", [Response]);

snapshot_command(_) ->
    io:format(standard_error, "Error: Use --list, --info ID, --delete ID, or --clone ID --type TYPE~n", []),
    halt(1).

get_clone_type([]) -> undefined;
get_clone_type(["--type", Type | _]) -> Type;
get_clone_type([_ | Rest]) -> get_clone_type(Rest).

get_clone_name([]) -> undefined;
get_clone_name(["--name", Name | _]) -> Name;
get_clone_name([_ | Rest]) -> get_clone_name(Rest).

get_clone_shell([]) -> undefined;
get_clone_shell(["--shell", Shell | _]) -> Shell;
get_clone_shell([_ | Rest]) -> get_clone_shell(Rest).

get_clone_ports([]) -> undefined;
get_clone_ports(["--ports", Ports | _]) -> Ports;
get_clone_ports([_ | Rest]) -> get_clone_ports(Rest).

build_clone_json(undefined, _, _, _) ->
    io:format(standard_error, "\033[31mError: --type required (session or service)\033[0m~n", []),
    halt(1);
build_clone_json(Type, Name, Shell, Ports) ->
    TypeJson = "{\"type\":\"" ++ Type ++ "\"",
    NameJson = case Name of
        undefined -> "";
        N -> ",\"name\":\"" ++ escape_json(N) ++ "\""
    end,
    ShellJson = case Shell of
        undefined -> "";
        S -> ",\"shell\":\"" ++ S ++ "\""
    end,
    PortsJson = case Ports of
        undefined -> "";
        P -> ",\"ports\":[" ++ P ++ "]"
    end,
    TypeJson ++ NameJson ++ ShellJson ++ PortsJson ++ "}".

%% Image command
image_command(["--list" | _]) ->
    image_command(["-l"]);
image_command(["-l" | _]) ->
    {PublicKey, SecretKey} = get_api_keys(),
    Result = api_request("/images", "GET", "", PublicKey, SecretKey),
    io:format("~s~n", [Result]),
    halt(0);

image_command(["--info", ImageId | _]) ->
    {PublicKey, SecretKey} = get_api_keys(),
    Result = api_request("/images/" ++ ImageId, "GET", "", PublicKey, SecretKey),
    io:format("~s~n", [Result]),
    halt(0);

image_command(["--delete", ImageId | _]) ->
    ApiKey = get_api_key(),
    case curl_delete_with_sudo(ApiKey, "/images/" ++ ImageId) of
        {ok, _, _} ->
            io:format("\033[32mImage deleted: ~s\033[0m~n", [ImageId]),
            halt(0);
        {ok, _} ->
            io:format("\033[32mImage deleted: ~s\033[0m~n", [ImageId]),
            halt(0);
        {error, cancelled} ->
            halt(1);
        {error, Msg} ->
            io:format(standard_error, "\033[31mError: ~s\033[0m~n", [Msg]),
            halt(1)
    end;

image_command(["--lock", ImageId | _]) ->
    {PublicKey, SecretKey} = get_api_keys(),
    api_request("/images/" ++ ImageId ++ "/lock", "POST", "", PublicKey, SecretKey),
    io:format("\033[32mImage locked: ~s\033[0m~n", [ImageId]),
    halt(0);

image_command(["--unlock", ImageId | _]) ->
    ApiKey = get_api_key(),
    case curl_post_with_sudo(ApiKey, "/images/" ++ ImageId ++ "/unlock", "{}") of
        {ok, _, _} ->
            io:format("\033[32mImage unlocked: ~s\033[0m~n", [ImageId]),
            halt(0);
        {ok, _} ->
            io:format("\033[32mImage unlocked: ~s\033[0m~n", [ImageId]),
            halt(0);
        {error, cancelled} ->
            halt(1);
        {error, Msg} ->
            io:format(standard_error, "\033[31mError: ~s\033[0m~n", [Msg]),
            halt(1)
    end;

image_command(["--publish", SourceId | Rest]) ->
    SourceType = get_image_source_type(Rest),
    case SourceType of
        undefined ->
            io:format(standard_error, "\033[31mError: --source-type required (service or snapshot)\033[0m~n", []),
            halt(1);
        _ ->
            {PublicKey, SecretKey} = get_api_keys(),
            Name = get_image_name(Rest),
            Payload = build_publish_json(SourceType, SourceId, Name),
            Result = api_request("/images/publish", "POST", Payload, PublicKey, SecretKey),
            io:format("\033[32mImage published\033[0m~n"),
            io:format("~s~n", [Result]),
            halt(0)
    end;

image_command(["--visibility", ImageId, Mode | _]) ->
    {PublicKey, SecretKey} = get_api_keys(),
    Payload = "{\"visibility\":\"" ++ Mode ++ "\"}",
    api_request("/images/" ++ ImageId ++ "/visibility", "POST", Payload, PublicKey, SecretKey),
    io:format("\033[32mImage visibility set to ~s\033[0m~n", [Mode]),
    halt(0);

image_command(["--spawn", ImageId | Rest]) ->
    {PublicKey, SecretKey} = get_api_keys(),
    Name = get_image_name(Rest),
    Ports = get_image_ports(Rest),
    Payload = build_spawn_json(Name, Ports),
    Result = api_request("/images/" ++ ImageId ++ "/spawn", "POST", Payload, PublicKey, SecretKey),
    io:format("\033[32mService spawned from image\033[0m~n"),
    io:format("~s~n", [Result]),
    halt(0);

image_command(["--clone", ImageId | Rest]) ->
    {PublicKey, SecretKey} = get_api_keys(),
    Name = get_image_name(Rest),
    Payload = case Name of
        undefined -> "{}";
        N -> "{\"name\":\"" ++ escape_json(N) ++ "\"}"
    end,
    Result = api_request("/images/" ++ ImageId ++ "/clone", "POST", Payload, PublicKey, SecretKey),
    io:format("\033[32mImage cloned\033[0m~n"),
    io:format("~s~n", [Result]),
    halt(0);

image_command(_) ->
    io:format(standard_error, "Error: Use --list, --info ID, --delete ID, --lock ID, --unlock ID, --publish ID, --visibility ID MODE, --spawn ID, or --clone ID~n", []),
    halt(1).

get_image_source_type([]) -> undefined;
get_image_source_type(["--source-type", Type | _]) -> Type;
get_image_source_type([_ | Rest]) -> get_image_source_type(Rest).

get_image_name([]) -> undefined;
get_image_name(["--name", Name | _]) -> Name;
get_image_name([_ | Rest]) -> get_image_name(Rest).

get_image_ports([]) -> undefined;
get_image_ports(["--ports", Ports | _]) -> Ports;
get_image_ports([_ | Rest]) -> get_image_ports(Rest).

build_publish_json(SourceType, SourceId, Name) ->
    Base = "{\"source_type\":\"" ++ SourceType ++ "\",\"source_id\":\"" ++ SourceId ++ "\"",
    NameJson = case Name of
        undefined -> "";
        N -> ",\"name\":\"" ++ escape_json(N) ++ "\""
    end,
    Base ++ NameJson ++ "}".

build_spawn_json(Name, Ports) ->
    Parts = [],
    Parts1 = case Name of
        undefined -> Parts;
        N -> ["\"name\":\"" ++ escape_json(N) ++ "\"" | Parts]
    end,
    Parts2 = case Ports of
        undefined -> Parts1;
        P -> ["\"ports\":[" ++ P ++ "]" | Parts1]
    end,
    "{" ++ string:join(lists:reverse(Parts2), ",") ++ "}".


%% Get languages cache path
get_languages_cache_path() ->
    Home = os:getenv("HOME"),
    case Home of
        false -> "/tmp/.unsandbox/languages.json";
        _ -> Home ++ "/.unsandbox/languages.json"
    end.

%% Load languages from cache
load_languages_cache() ->
    CachePath = get_languages_cache_path(),
    case file:read_file(CachePath) of
        {ok, Bin} ->
            Content = binary_to_list(Bin),
            Timestamp = extract_json_number(Content, "timestamp"),
            Now = erlang:system_time(second),
            if
                Timestamp > 0 andalso (Now - Timestamp) < ?LANGUAGES_CACHE_TTL ->
                    extract_json_array(Content, "languages");
                true ->
                    undefined
            end;
        {error, _} ->
            undefined
    end.

%% Save languages to cache
save_languages_cache(Languages) ->
    CachePath = get_languages_cache_path(),
    CacheDir = filename:dirname(CachePath),
    filelib:ensure_dir(CachePath),
    file:make_dir(CacheDir),
    Timestamp = erlang:system_time(second),
    LanguagesJson = "[" ++ string:join(["\"" ++ L ++ "\"" || L <- Languages], ",") ++ "]",
    Json = "{\"languages\":" ++ LanguagesJson ++ ",\"timestamp\":" ++ integer_to_list(Timestamp) ++ "}",
    file:write_file(CachePath, Json).

%% Extract JSON number field
extract_json_number(Json, Field) ->
    Pattern = "\"" ++ Field ++ "\":",
    case string:str(Json, Pattern) of
        0 -> 0;
        Pos ->
            Start = Pos + length(Pattern),
            Rest = lists:nthtail(Start - 1, Json),
            extract_number(Rest)
    end.

extract_number(Str) ->
    extract_number(Str, []).

extract_number([], Acc) ->
    case Acc of
        [] -> 0;
        _ -> list_to_integer(lists:reverse(Acc))
    end;
extract_number([C | Rest], Acc) when C >= $0, C =< $9 ->
    extract_number(Rest, [C | Acc]);
extract_number(_, Acc) ->
    case Acc of
        [] -> 0;
        _ -> list_to_integer(lists:reverse(Acc))
    end.

%% Languages command
languages_command(Args) ->
    JsonOutput = lists:member("--json", Args),

    %% Try to load from cache first
    Languages = case load_languages_cache() of
        undefined ->
            %% Cache miss or expired, fetch from API
            ApiKey = get_api_key(),
            Response = curl_get(ApiKey, "/languages"),
            Langs = extract_json_array(Response, "languages"),
            save_languages_cache(Langs),
            Langs;
        CachedLanguages ->
            CachedLanguages
    end,

    if
        JsonOutput ->
            %% Output raw JSON array
            case Languages of
                [] -> io:format("[]~n");
                _ -> io:format("[~s]~n", [string:join(["\"" ++ L ++ "\"" || L <- Languages], ",")])
            end;
        true ->
            %% Output one language per line
            case Languages of
                [] -> ok;
                _ -> [io:format("~s~n", [L]) || L <- Languages]
            end
    end.

%% Key command
key_command(Args) ->
    case lists:member("--rotate", Args) of
        true -> key_rotate(Args);
        false -> key_command_validate(Args)
    end.

key_command_validate(Args) ->
    {PublicKey, SecretKey} = get_api_keys(),
    case has_extend_flag(Args) of
        true ->
            open_extend_page(PublicKey);
        false ->
            {Code, Resp} = key_validate_post(PublicKey, SecretKey),
            display_key_result(Code, Resp, PublicKey)
    end.

%% POST /keys/validate, empty body, signed; headers go through a 0600 file.
key_validate_post(PublicKey, SecretKey) ->
    Path = "/keys/validate",
    Timestamp = integer_to_list(erlang:system_time(second)),
    SigLines = case SecretKey of
        false -> [];
        _ -> ["X-Timestamp: " ++ Timestamp,
              "X-Signature: " ++ make_signature(SecretKey, Timestamp, "POST", Path, "")]
    end,
    HeaderLines = ["Authorization: Bearer " ++ PublicKey,
                   "Content-Type: application/json"] ++ SigLines,
    HdrFile = private_temp_file("hdr", string:join(HeaderLines, "\n") ++ "\n"),
    Cmd = "curl -s -X POST -w '\\n%{http_code}' -H @" ++ sh_quote(HdrFile) ++
          " " ++ sh_quote(?API_BASE ++ Path),
    Output = os:cmd(Cmd),
    file:delete(HdrFile),
    case lists:reverse(string:split(Output, "\n", all)) of
        [StatusStr | BodyRev] ->
            Code = try list_to_integer(string:trim(StatusStr)) catch _:_ -> 0 end,
            {Code, string:join(lists:reverse(BodyRev), "\n")};
        _ -> {0, Output}
    end.

json_int_field(Json, Field) ->
    case re:run(Json, "\"" ++ Field ++ "\"\\s*:\\s*(-?[0-9]+)", [{capture, [1], list}]) of
        {match, [V]} -> list_to_integer(V);
        nomatch -> undefined
    end.

display_key_result(Code, Resp, PublicKey) ->
    {State, Reason} =
        case Code of
            200 ->
                St = case json_string_field(Resp, "status") of
                    "" -> case string:str(Resp, "\"valid\":true") of 0 -> "invalid"; _ -> "active" end;
                    S -> S
                end,
                {St, "status " ++ St};
            401 ->
                Err = case json_string_field(Resp, "error") of "" -> "unauthorized"; E -> E end,
                St = case Err of
                    ":expired" -> "expired"; "expired" -> "expired";
                    ":suspended" -> "suspended"; "suspended" -> "suspended";
                    _ -> "invalid"
                end,
                {St, Err};
            0 ->
                io:format(standard_error, "\033[31mError: request to ~s/keys/validate failed\033[0m~n", [?API_BASE]),
                halt(1);
            _ ->
                Msg = case json_string_field(Resp, "error") of "" -> json_string_field(Resp, "message"); M -> M end,
                io:format(standard_error, "\033[31mError: HTTP ~p ~s\033[0m~n", [Code, Msg]),
                halt(4)
        end,
    case State of
        "active" ->
            Tier = case json_int_field(Resp, "tier") of undefined -> "N/A"; T -> integer_to_list(T) end,
            Status = case json_string_field(Resp, "status") of "" -> "N/A"; S2 -> S2 end,
            {Expires, Remaining} =
                case json_int_field(Resp, "valid_through") of
                    undefined -> {"N/A", "N/A"};
                    Vt ->
                        Secs = Vt div 1000,
                        Exp = calendar:system_time_to_rfc3339(Secs, [{offset, "Z"}, {unit, second}]),
                        Left = Secs - erlang:system_time(second),
                        Rem = if
                            Left =< 0 -> "expired";
                            true -> lists:flatten(io_lib:format("~bd ~bh ~bm",
                                        [Left div 86400, (Left rem 86400) div 3600, (Left rem 3600) div 60]))
                        end,
                        {Exp, Rem}
                end,
            io:format("\033[32mValid\033[0m~n"),
            io:format("Public Key: ~s~n", [PublicKey]),
            io:format("Tier: ~s~n", [Tier]),
            io:format("Status: ~s~n", [Status]),
            io:format("Expires: ~s~n", [Expires]),
            io:format("Time Remaining: ~s~n", [Remaining]);
        "expired" ->
            io:format("\033[31mExpired\033[0m~n"),
            io:format("Public Key: ~s~n", [PublicKey]),
            io:format("\033[33mTo renew: Visit ~s/keys/extend\033[0m~n", [?PORTAL_BASE]),
            halt(3);
        "suspended" ->
            io:format("\033[31mSuspended\033[0m~n"),
            io:format("Public Key: ~s~n", [PublicKey]),
            halt(3);
        _ ->
            io:format("\033[31mInvalid\033[0m~n"),
            io:format("Public Key: ~s~n", [PublicKey]),
            io:format("Reason: ~s~n", [Reason]),
            halt(3)
    end.

open_extend_page(PublicKey) ->
    Url = "https://unsandbox.com/keys/extend?pk=" ++ PublicKey,
    io:format("\033[33mOpening browser to extend key...\033[0m~n"),
    case os:type() of
        {unix, darwin} ->
            os:cmd("open " ++ sh_quote(Url));
        {unix, _} ->
            os:cmd("xdg-open " ++ sh_quote(Url) ++ " 2>/dev/null || sensible-browser " ++ sh_quote(Url) ++ " 2>/dev/null &");
        {win32, _} ->
            os:cmd("start " ++ Url)
    end.

%% Helpers

%% @doc Load credentials from a CSV file at the given path.
%% Skips blank lines and comment lines (#). Returns {ok, {PK, SK}} or error.
load_credentials_from_csv(CsvPath, AccountIndex) ->
    case file:read_file(CsvPath) of
        {ok, Bin} ->
            Lines = string:split(binary_to_list(Bin), "\n", all),
            ValidAccounts = lists:filtermap(fun(Line) ->
                Trimmed = string:trim(Line),
                case Trimmed of
                    "" -> false;
                    [$# | _] -> false;
                    _ ->
                        Parts = string:split(Trimmed, ",", all),
                        case Parts of
                            [PK, SK | _] ->
                                PKt = string:trim(PK),
                                SKt = string:trim(SK),
                                if
                                    length(PKt) > 8 andalso length(SKt) > 8 ->
                                        {true, {PKt, SKt}};
                                    true -> false
                                end;
                            _ -> false
                        end
                end
            end, Lines),
            if
                AccountIndex < length(ValidAccounts) ->
                    {ok, lists:nth(AccountIndex + 1, ValidAccounts)};
                true ->
                    error
            end;
        _ ->
            error
    end.

%% @doc Resolve credentials with correct priority:
%%   1. --account N process-dict override -> accounts.csv row N
%%   2. UNSANDBOX_PUBLIC_KEY / UNSANDBOX_SECRET_KEY env vars
%%   3. ~/.unsandbox/accounts.csv row 0 (or UNSANDBOX_ACCOUNT env)
%%   4. ./accounts.csv row 0
get_api_keys() ->
    Home = case os:getenv("HOME") of false -> "."; H -> H end,
    HomeCsv = filename:join([Home, ".unsandbox", "accounts.csv"]),
    %% Priority 1: explicit --account N (stored in process dict by main/1)
    case erlang:get(account_index) of
        undefined ->
            %% Priority 2: environment variables
            PublicKey = os:getenv("UNSANDBOX_PUBLIC_KEY"),
            SecretKey = os:getenv("UNSANDBOX_SECRET_KEY"),
            ApiKey = os:getenv("UNSANDBOX_API_KEY"),
            if
                PublicKey =/= false andalso SecretKey =/= false ->
                    {PublicKey, SecretKey};
                ApiKey =/= false ->
                    {ApiKey, false};
                true ->
                    %% Priority 3: ~/.unsandbox/accounts.csv (or UNSANDBOX_ACCOUNT index)
                    DefaultIndex = case os:getenv("UNSANDBOX_ACCOUNT") of
                        false -> 0;
                        IdxStr -> try list_to_integer(string:trim(IdxStr)) catch _:_ -> 0 end
                    end,
                    case load_credentials_from_csv(HomeCsv, DefaultIndex) of
                        {ok, {PK, SK}} ->
                            {PK, SK};
                        error ->
                            %% Priority 4: ./accounts.csv
                            case load_credentials_from_csv("accounts.csv", DefaultIndex) of
                                {ok, {PK2, SK2}} ->
                                    {PK2, SK2};
                                error ->
                                    io:format("Error: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set (or UNSANDBOX_API_KEY for backwards compat)~n"),
                                    halt(1)
                            end
                    end
            end;
        AccountIndex ->
            case load_credentials_from_csv(HomeCsv, AccountIndex) of
                {ok, {PK, SK}} ->
                    {PK, SK};
                error ->
                    case load_credentials_from_csv("accounts.csv", AccountIndex) of
                        {ok, {PK2, SK2}} ->
                            {PK2, SK2};
                        error ->
                            io:format("Error: No credentials found for account index ~B in accounts.csv~n", [AccountIndex]),
                            halt(1)
                    end
            end
    end.

get_api_key() ->
    {PublicKey, _} = get_api_keys(),
    PublicKey.

%% API base URL: UNSANDBOX_API_URL (default https://api.unsandbox.com), no trailing slash
api_base() ->
    case os:getenv("UNSANDBOX_API_URL") of
        false -> "https://api.unsandbox.com";
        "" -> "https://api.unsandbox.com";
        Url -> string:trim(Url, trailing, "/")
    end.

%% The accounts.csv the credentials came from, or undefined (environment
%% variables). Mirrors get_api_keys/0 tiers.
credentials_csv_path() ->
    Home = case os:getenv("HOME") of false -> "."; H -> H end,
    HomeCsv = filename:join([Home, ".unsandbox", "accounts.csv"]),
    Index = case erlang:get(account_index) of
        undefined ->
            EnvPair = os:getenv("UNSANDBOX_PUBLIC_KEY") =/= false andalso
                      os:getenv("UNSANDBOX_SECRET_KEY") =/= false,
            case EnvPair orelse os:getenv("UNSANDBOX_API_KEY") =/= false of
                true -> undefined;
                false ->
                    case os:getenv("UNSANDBOX_ACCOUNT") of
                        false -> 0;
                        IdxStr -> try list_to_integer(string:trim(IdxStr)) catch _:_ -> 0 end
                    end
            end;
        N -> N
    end,
    case Index of
        undefined -> undefined;
        _ ->
            Found = [P || P <- [HomeCsv, "accounts.csv"],
                          load_credentials_from_csv(P, Index) =/= error],
            case Found of [First | _] -> First; [] -> undefined end
    end.

%% Replace the secret on the line whose first field equals PublicKey. Every other
%% line, field, comment and the order stay as they are. Temp file (0600) + rename.
rewrite_accounts_csv(Path, PublicKey, NewSecret) ->
    {ok, Bin} = file:read_file(Path),
    Lines = string:split(binary_to_list(Bin), "\n", all),
    {NewLines, Changed} = lists:mapfoldl(fun(Line, Done) ->
        Trimmed = string:trim(Line),
        Fields = string:split(Line, ",", all),
        IsTarget = (not Done) andalso Trimmed =/= "" andalso
                   not lists:prefix("#", Trimmed) andalso length(Fields) >= 2 andalso
                   string:trim(hd(Fields)) == PublicKey,
        case IsTarget of
            true ->
                [First, _Old | Rest] = Fields,
                {string:join([First, NewSecret | Rest], ","), true};
            false -> {Line, Done}
        end
    end, false, Lines),
    case Changed of
        false -> false;
        true ->
            Tmp = filename:join(filename:dirname(Path),
                                "." ++ filename:basename(Path) ++ "." ++ os:getpid() ++ ".tmp"),
            ok = file:write_file(Tmp, <<>>),
            ok = file:change_mode(Tmp, 8#600),
            ok = file:write_file(Tmp, string:join(NewLines, "\n")),
            ok = file:rename(Tmp, Path),
            true
    end.

stdin_is_tty() ->
    case file:read_link("/proc/self/fd/0") of
        {ok, Target} -> lists:prefix("/dev/pts/", Target) orelse lists:prefix("/dev/tty", Target);
        _ -> false
    end.

%% POST /keys/self/rotate-secret, body {}; headers go through a 0600 file, never argv.
rotate_post(PublicKey, SecretKey, Otp, Challenge) ->
    Path = "/keys/self/rotate-secret",
    Body = "{}",
    Timestamp = integer_to_list(erlang:system_time(second)),
    Signature = make_signature(SecretKey, Timestamp, "POST", Path, Body),
    SudoLines = case Otp of
        undefined -> [];
        _ -> ["X-Sudo-OTP: " ++ Otp, "X-Sudo-Challenge: " ++ Challenge]
    end,
    HeaderLines = ["Authorization: Bearer " ++ PublicKey,
                   "X-Timestamp: " ++ Timestamp,
                   "X-Signature: " ++ Signature,
                   "Content-Type: application/json"] ++ SudoLines,
    HdrFile = private_temp_file("hdr", string:join(HeaderLines, "\n") ++ "\n"),
    Cmd = "curl -s -X POST -w '\\n%{http_code}' -H @" ++ sh_quote(HdrFile) ++
          " --data-raw '{}' " ++ sh_quote(?API_BASE ++ Path),
    Output = os:cmd(Cmd),
    file:delete(HdrFile),
    case lists:reverse(string:split(Output, "\n", all)) of
        [StatusStr | BodyRev] ->
            Code = try list_to_integer(string:trim(StatusStr)) catch _:_ -> 0 end,
            {Code, string:join(lists:reverse(BodyRev), "\n")};
        _ -> {0, Output}
    end.

%% String field from a JSON object, tolerant of whitespace around the colon.
json_string_field(Json, Field) ->
    case re:run(Json, "\"" ++ Field ++ "\"\\s*:\\s*\"([^\"]*)\"", [{capture, [1], list}]) of
        {match, [Value]} -> Value;
        nomatch -> ""
    end.

rotate_fail(Code, Resp) ->
    Error = json_string_field(Resp, "error"),
    if
        Code == 403 andalso Error == "email_required" ->
            io:format(standard_error, "Error: this key has no email; add one or use POST /keys/recover~n", []),
            halt(3);
        Code == 401 ->
            io:format(standard_error, "Error: authentication failed~n", []),
            halt(3);
        true ->
            Msg = case {json_string_field(Resp, "message"), Error} of
                {"", ""} -> "HTTP " ++ integer_to_list(Code);
                {"", E} -> E;
                {M, _} -> M
            end,
            io:format(standard_error, "Error: ~s~n", [Msg]),
            halt(4)
    end.

get_otp_arg([]) -> undefined;
get_otp_arg(["--otp", Code | _]) -> string:trim(Code);
get_otp_arg([_ | Rest]) -> get_otp_arg(Rest).

%% un key --rotate [--otp CODE]: rotate this key's secret (see CLI_SPEC.md)
key_rotate(Args) ->
    {PublicKey, SecretKey} = get_api_keys(),
    case SecretKey of
        false ->
            io:format(standard_error, "Error: authentication failed~n", []),
            halt(3);
        _ -> ok
    end,
    Home = case os:getenv("HOME") of false -> "."; H -> H end,
    UnsandboxDir = filename:join([Home, ".unsandbox"]),
    Hash = lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(crypto:hash(sha256, PublicKey))]),
    ChallengeFile = filename:join([UnsandboxDir, "rotate-" ++ lists:sublist(Hash, 16) ++ ".challenge"]),
    {Code, Resp} = case get_otp_arg(Args) of
        undefined ->
            case rotate_post(PublicKey, SecretKey, undefined, undefined) of
                {428, Body428} ->
                    Challenge = json_string_field(Body428, "challenge_id"),
                    case stdin_is_tty() of
                        true ->
                            io:format(standard_error, "One-time code emailed to the key owner.~n", []),
                            io:format(standard_error, "Code: ", []),
                            Otp = case io:get_line("") of
                                L when is_list(L) -> string:trim(L);
                                _ -> ""
                            end,
                            case Otp of
                                "" ->
                                    io:format(standard_error, "Error: cancelled~n", []),
                                    halt(1);
                                _ -> rotate_post(PublicKey, SecretKey, Otp, Challenge)
                            end;
                        false ->
                            ok = filelib:ensure_dir(ChallengeFile),
                            ok = file:change_mode(UnsandboxDir, 8#700),
                            ok = file:write_file(ChallengeFile, <<>>),
                            ok = file:change_mode(ChallengeFile, 8#600),
                            ok = file:write_file(ChallengeFile, Challenge),
                            io:format("One-time code emailed to the key owner. Finish with: un key --rotate --otp CODE~n"),
                            halt(0)
                    end;
                Other -> Other
            end;
        OtpArg ->
            case file:read_file(ChallengeFile) of
                {ok, ChBin} ->
                    Result = rotate_post(PublicKey, SecretKey, OtpArg, string:trim(binary_to_list(ChBin))),
                    file:delete(ChallengeFile),
                    Result;
                _ ->
                    io:format(standard_error, "Error: no rotation in progress; run: un key --rotate~n", []),
                    halt(2)
            end
    end,
    case Code of
        200 -> ok;
        _ -> rotate_fail(Code, Resp)
    end,
    case json_string_field(Resp, "secret") of
        "" ->
            io:format(standard_error, "Error: API returned no secret~n", []),
            halt(4);
        NewSecret ->
            CsvPath = credentials_csv_path(),
            Updated = CsvPath =/= undefined andalso rewrite_accounts_csv(CsvPath, PublicKey, NewSecret),
            case Updated of
                true ->
                    io:format("Secret rotated. Updated ~s (public key ~s).~n", [CsvPath, PublicKey]);
                false ->
                    io:format("New secret (shown once, store it now): ~s~n", [NewSecret]),
                    io:format("Warning: UNSANDBOX_SECRET_KEY / -k still hold the old, now invalid, secret.~n")
            end
    end.

hmac_sha256(Secret, Message) ->
    string:lowercase(
        lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(crypto:mac(hmac, sha256, Secret, Message))])
    ).

make_signature(SecretKey, Timestamp, Method, Path, Body) ->
    %% Server verifies against the request path without its query string (conn.request_path)
    SignPath = lists:takewhile(fun(C) -> C =/= $? end, Path),
    Message = Timestamp ++ ":" ++ Method ++ ":" ++ SignPath ++ ":" ++ Body,
    hmac_sha256(SecretKey, Message).

check_clock_drift_error(Response) ->
    HasTimestamp = string:str(Response, "timestamp") > 0 orelse string:str(Response, "\"timestamp\"") > 0,
    Has401 = string:str(Response, "401") > 0,
    HasExpired = string:str(Response, "expired") > 0,
    HasInvalid = string:str(Response, "invalid") > 0,

    case HasTimestamp andalso (Has401 orelse HasExpired orelse HasInvalid) of
        true ->
            io:format(standard_error, "\033[31mError: Request timestamp expired (must be within 5 minutes of server time)\033[0m~n", []),
            io:format(standard_error, "\033[33mYour computer's clock may have drifted.\033[0m~n", []),
            io:format(standard_error, "Check your system time and sync with NTP if needed:~n", []),
            io:format(standard_error, "  Linux:   sudo ntpdate -s time.nist.gov~n", []),
            io:format(standard_error, "  macOS:   sudo sntp -sS time.apple.com~n", []),
            io:format(standard_error, "  Windows: w32tm /resync~n", []),
            halt(1);
        false ->
            ok
    end.

%% Header lines that authenticate one request (signed when a secret key is known).
auth_lines(PublicKey, false, _Method, _Path, _Body) ->
    ["Authorization: Bearer " ++ PublicKey];
auth_lines(PublicKey, SecretKey, Method, Path, Body) ->
    Timestamp = integer_to_list(erlang:system_time(second)),
    ["Authorization: Bearer " ++ PublicKey,
     "X-Timestamp: " ++ Timestamp,
     "X-Signature: " ++ make_signature(SecretKey, Timestamp, Method, Path, Body)].

%% Every authenticated request goes through here. Headers and body travel in
%% 0600 files, never argv (where `ps` shows them); --data-binary sends the body
%% byte-for-byte as signed (-d @file strips newlines); every shell word is quoted.
%% Opts: content_type, headers (extra lines), curl_args (extra curl flags).
signed_curl({PublicKey, SecretKey}, Method, Endpoint, Body, Opts) ->
    BodyStr = case Body of undefined -> ""; _ -> Body end,
    TypeLines = case Body of
        undefined -> [];
        _ -> ["Content-Type: " ++ maps:get(content_type, Opts, "application/json")]
    end,
    HeaderLines = auth_lines(PublicKey, SecretKey, Method, Endpoint, BodyStr)
                  ++ maps:get(headers, Opts, []) ++ TypeLines,
    HdrFile = private_temp_file("hdr", string:join(HeaderLines, "\n") ++ "\n"),
    BodyFile = case Body of undefined -> undefined; _ -> private_temp_file("body", Body) end,
    BodyArgs = case BodyFile of
        undefined -> "";
        _ -> " --data-binary @" ++ sh_quote(BodyFile)
    end,
    Cmd = "curl -s -X " ++ Method ++ maps:get(curl_args, Opts, "") ++
          " -H @" ++ sh_quote(HdrFile) ++ BodyArgs ++ " " ++ sh_quote(?API_BASE ++ Endpoint),
    try
        os:cmd(Cmd)
    after
        file:delete(HdrFile),
        BodyFile =/= undefined andalso file:delete(BodyFile)
    end.

%% A fresh file only this user can read: unguessable name, created exclusively
%% (never follows a planted symlink), mode 0600 before any content lands.
private_temp_file(Kind, Data) ->
    Dir = case os:getenv("TMPDIR") of
        false -> "/tmp";
        "" -> "/tmp";
        D -> D
    end,
    Rand = lists:flatten([io_lib:format("~2.16.0b", [X]) || X <- binary_to_list(crypto:strong_rand_bytes(12))]),
    Path = filename:join(Dir, "un_erl_" ++ Kind ++ "_" ++ Rand),
    ok = file:write_file(Path, <<>>, [exclusive]),
    ok = file:change_mode(Path, 8#600),
    ok = file:write_file(Path, Data),
    Path.

%% Single-quote one shell word.
sh_quote(S) ->
    "'" ++ lists:flatten(string:replace(S, "'", "'\\''", all)) ++ "'".

ext_to_lang(".hs") -> {ok, "haskell"};
ext_to_lang(".ml") -> {ok, "ocaml"};
ext_to_lang(".clj") -> {ok, "clojure"};
ext_to_lang(".scm") -> {ok, "scheme"};
ext_to_lang(".lisp") -> {ok, "commonlisp"};
ext_to_lang(".erl") -> {ok, "erlang"};
ext_to_lang(".ex") -> {ok, "elixir"};
ext_to_lang(".exs") -> {ok, "elixir"};
ext_to_lang(".py") -> {ok, "python"};
ext_to_lang(".js") -> {ok, "javascript"};
ext_to_lang(".ts") -> {ok, "typescript"};
ext_to_lang(".rb") -> {ok, "ruby"};
ext_to_lang(".go") -> {ok, "go"};
ext_to_lang(".rs") -> {ok, "rust"};
ext_to_lang(".c") -> {ok, "c"};
ext_to_lang(".cpp") -> {ok, "cpp"};
ext_to_lang(".cc") -> {ok, "cpp"};
ext_to_lang(".java") -> {ok, "java"};
ext_to_lang(".kt") -> {ok, "kotlin"};
ext_to_lang(".cs") -> {ok, "csharp"};
ext_to_lang(".fs") -> {ok, "fsharp"};
ext_to_lang(".jl") -> {ok, "julia"};
ext_to_lang(".r") -> {ok, "r"};
ext_to_lang(".cr") -> {ok, "crystal"};
ext_to_lang(".d") -> {ok, "d"};
ext_to_lang(".nim") -> {ok, "nim"};
ext_to_lang(".zig") -> {ok, "zig"};
ext_to_lang(".v") -> {ok, "v"};
ext_to_lang(".dart") -> {ok, "dart"};
ext_to_lang(".sh") -> {ok, "bash"};
ext_to_lang(".pl") -> {ok, "perl"};
ext_to_lang(".lua") -> {ok, "lua"};
ext_to_lang(".php") -> {ok, "php"};
ext_to_lang(Ext) -> {error, Ext}.

escape_json(Str) ->
    escape_json(Str, []).

escape_json([], Acc) ->
    lists:reverse(Acc);
escape_json([$\\ | Rest], Acc) ->
    escape_json(Rest, [$\\, $\\ | Acc]);
escape_json([$\" | Rest], Acc) ->
    escape_json(Rest, [$\", $\\ | Acc]);
escape_json([$\n | Rest], Acc) ->
    escape_json(Rest, [$n, $\\ | Acc]);
escape_json([$\r | Rest], Acc) ->
    escape_json(Rest, [$r, $\\ | Acc]);
escape_json([$\t | Rest], Acc) ->
    escape_json(Rest, [$t, $\\ | Acc]);
escape_json([C | Rest], Acc) ->
    escape_json(Rest, [C | Acc]).

build_json(Language, Code) ->
    "{\"language\":\"" ++ Language ++ "\",\"code\":\"" ++ escape_json(Code) ++ "\"}".

read_and_base64(Filepath) ->
    case file:read_file(Filepath) of
        {ok, Content} ->
            base64:encode_to_string(Content);
        {error, _} ->
            ""
    end.

build_input_files_json([]) -> "";
build_input_files_json(Files) ->
    FileJsons = lists:map(fun(F) ->
        B64 = read_and_base64(F),
        Basename = filename:basename(F),
        "{\"filename\":\"" ++ escape_json(Basename) ++ "\",\"content\":\"" ++ B64 ++ "\"}"
    end, Files),
    ",\"input_files\":[" ++ string:join(FileJsons, ",") ++ "]".

write_temp_file(Data) ->
    private_temp_file("body", Data).

curl_post(ApiKey, Endpoint, TmpFile) ->
    {ok, Body} = file:read_file(TmpFile),
    {_, SecretKey} = get_api_keys(),
    Result = signed_curl({ApiKey, SecretKey}, "POST", Endpoint, binary_to_list(Body), #{}),
    check_clock_drift_error(Result),
    Result.

curl_get(ApiKey, Endpoint) ->
    {_, SecretKey} = get_api_keys(),
    Result = signed_curl({ApiKey, SecretKey}, "GET", Endpoint, undefined, #{}),
    check_clock_drift_error(Result),
    Result.

curl_delete(ApiKey, Endpoint) ->
    {_, SecretKey} = get_api_keys(),
    Result = signed_curl({ApiKey, SecretKey}, "DELETE", Endpoint, undefined, #{}),
    check_clock_drift_error(Result),
    Result.

%% Handle 428 sudo OTP challenge - prompts user for OTP and retries the request
handle_sudo_challenge(Response, Method, Endpoint, Body) ->
    ChallengeId = extract_json_field(Response, "challenge_id"),
    io:format(standard_error, "\033[33mConfirmation required. Check your email for a one-time code.\033[0m~n", []),
    io:format(standard_error, "Enter OTP: ", []),
    case io:get_line("") of
        eof ->
            io:format(standard_error, "Error: Failed to read OTP~n", []),
            {error, cancelled};
        OtpRaw ->
            Otp = string:trim(OtpRaw),
            case Otp of
                "" ->
                    io:format(standard_error, "Error: Operation cancelled~n", []),
                    {error, cancelled};
                _ ->
                    %% Retry the same request: same body, fresh signature, sudo headers.
                    ChallengeLines = case ChallengeId of
                        "" -> [];
                        _ -> ["X-Sudo-Challenge: " ++ ChallengeId]
                    end,
                    RetryBody = case Body of "" -> undefined; _ -> Body end,
                    RetryResult = signed_curl(get_api_keys(), Method, Endpoint, RetryBody,
                                              #{headers => ["X-Sudo-OTP: " ++ Otp] ++ ChallengeLines}),
                    case string:str(RetryResult, "\"error\"") of
                        0 -> {ok, RetryResult};
                        _ -> {error, RetryResult}
                    end
            end
    end.

%% Curl with 428 handling for destructive operations
curl_delete_with_sudo(ApiKey, Endpoint) ->
    {_, SecretKey} = get_api_keys(),
    Result = signed_curl({ApiKey, SecretKey}, "DELETE", Endpoint, undefined,
                         #{curl_args => " -w '\\n%{http_code}'"}),
    sudo_response(Result, "DELETE", Endpoint, "").

curl_post_with_sudo(ApiKey, Endpoint, Json) ->
    {_, SecretKey} = get_api_keys(),
    Result = signed_curl({ApiKey, SecretKey}, "POST", Endpoint, Json,
                         #{curl_args => " -w '\\n%{http_code}'"}),
    sudo_response(Result, "POST", Endpoint, Json).

%% Split the trailing status line off; a 428 asks for the emailed OTP and retries.
sudo_response(Result, Method, Endpoint, Json) ->
    case lists:reverse(string:split(Result, "\n", all)) of
        [StatusCodeStr | BodyLinesRev] ->
            Body = string:join(lists:reverse(BodyLinesRev), "\n"),
            check_clock_drift_error(Body),
            case string:to_integer(string:trim(StatusCodeStr)) of
                {428, _} -> handle_sudo_challenge(Body, Method, Endpoint, Json);
                {StatusCode, _} when is_integer(StatusCode) -> {ok, Body, StatusCode};
                _ -> {ok, Result, 0}
            end;
        _ ->
            {ok, Result, 0}
    end.

curl_patch(ApiKey, Endpoint, TmpFile) ->
    {ok, Body} = file:read_file(TmpFile),
    {_, SecretKey} = get_api_keys(),
    Result = signed_curl({ApiKey, SecretKey}, "PATCH", Endpoint, binary_to_list(Body), #{}),
    check_clock_drift_error(Result),
    Result.

curl_put_text(Endpoint, Content) ->
    Result = signed_curl(get_api_keys(), "PUT", Endpoint, Content, #{content_type => "text/plain"}),
    check_clock_drift_error(Result),
    Result.

build_env_content(EnvVars, EnvFile) ->
    % Build env content from list of env vars and env file
    VarLines = EnvVars,
    FileLines = case EnvFile of
        undefined -> [];
        "" -> [];
        _ ->
            case file:read_file(EnvFile) of
                {ok, Bin} ->
                    Lines = string:split(binary_to_list(Bin), "\n", all),
                    [L || L <- Lines,
                          length(string:trim(L)) > 0,
                          not lists:prefix("#", string:trim(L))];
                {error, _} -> []
            end
    end,
    string:join(VarLines ++ FileLines, "\n").

service_env_status(ServiceId) ->
    ApiKey = get_api_key(),
    Endpoint = "/services/" ++ ServiceId ++ "/env",
    Response = curl_get(ApiKey, Endpoint),
    io:format("~s~n", [Response]).

cli_service_env_set(ServiceId, Content) ->
    Endpoint = "/services/" ++ ServiceId ++ "/env",
    Response = curl_put_text(Endpoint, Content),
    io:format("~s~n", [Response]).

cli_service_env_export(ServiceId) ->
    ApiKey = get_api_key(),
    Endpoint = "/services/" ++ ServiceId ++ "/env/export",
    TmpFile = write_temp_file("{}"),
    Response = curl_post(ApiKey, Endpoint, TmpFile),
    file:delete(TmpFile),
    case extract_json_field(Response, "content") of
        "" -> io:format("~s~n", [Response]);
        ContentStr -> io:format("~s", [ContentStr])
    end.

cli_service_env_delete(ServiceId) ->
    ApiKey = get_api_key(),
    _ = curl_delete(ApiKey, "/services/" ++ ServiceId ++ "/env"),
    io:format("\033[32mVault deleted: ~s\033[0m~n", [ServiceId]).

cli_service_resize(ServiceId, VcpuStr) ->
    ApiKey = get_api_key(),
    Vcpu = list_to_integer(VcpuStr),
    if
        Vcpu < 1 orelse Vcpu > 8 ->
            io:format(standard_error, "\033[31mError: --vcpu must be between 1 and 8\033[0m~n", []),
            halt(1);
        true -> ok
    end,
    Json = "{\"vcpu\":" ++ integer_to_list(Vcpu) ++ "}",
    TmpFile = write_temp_file(Json),
    _ = curl_patch(ApiKey, "/services/" ++ ServiceId, TmpFile),
    file:delete(TmpFile),
    Ram = Vcpu * 2,
    io:format("\033[32mService resized to ~B vCPU, ~B GB RAM\033[0m~n", [Vcpu, Ram]).

cli_service_set_unfreeze_on_demand(ServiceId, EnabledStr) ->
    ApiKey = get_api_key(),
    Enabled = case string:lowercase(EnabledStr) of
        "true" -> "true";
        "1" -> "true";
        "yes" -> "true";
        "on" -> "true";
        _ -> "false"
    end,
    Json = "{\"unfreeze_on_demand\":" ++ Enabled ++ "}",
    TmpFile = write_temp_file(Json),
    _ = curl_patch(ApiKey, "/services/" ++ ServiceId, TmpFile),
    file:delete(TmpFile),
    io:format("\033[32mService unfreeze_on_demand set to ~s: ~s\033[0m~n", [Enabled, ServiceId]).

%% Argument parsing
parse_exec_args([], Opts) ->
    {maps:get(file, Opts), Opts};
parse_exec_args([Arg | Rest], Opts) ->
    case Arg of
        "-" ++ _ -> parse_exec_args(Rest, Opts);
        _ -> {Arg, Opts}
    end.

get_shell_opt([], Default) -> Default;
get_shell_opt(["--shell", Shell | _], _) -> Shell;
get_shell_opt(["-s", Shell | _], _) -> Shell;
get_shell_opt([_ | Rest], Default) -> get_shell_opt(Rest, Default).

get_service_name([]) -> undefined;
get_service_name(["--name", Name | _]) -> Name;
get_service_name([_ | Rest]) -> get_service_name(Rest).

get_service_ports([]) -> undefined;
get_service_ports(["--ports", Ports | _]) -> Ports;
get_service_ports([_ | Rest]) -> get_service_ports(Rest).

get_service_bootstrap([]) -> undefined;
get_service_bootstrap(["--bootstrap", Bootstrap | _]) -> Bootstrap;
get_service_bootstrap([_ | Rest]) -> get_service_bootstrap(Rest).

get_service_bootstrap_file([]) -> undefined;
get_service_bootstrap_file(["--bootstrap-file", BootstrapFile | _]) -> BootstrapFile;
get_service_bootstrap_file([_ | Rest]) -> get_service_bootstrap_file(Rest).

get_service_type([]) -> undefined;
get_service_type(["--type", Type | _]) -> Type;
get_service_type([_ | Rest]) -> get_service_type(Rest).

get_input_files(Args) -> get_input_files(Args, []).

get_input_files([], Acc) -> lists:reverse(Acc);
get_input_files(["-f", File | Rest], Acc) -> get_input_files(Rest, [File | Acc]);
get_input_files([_ | Rest], Acc) -> get_input_files(Rest, Acc).

has_extend_flag([]) -> false;
has_extend_flag(["--extend" | _]) -> true;
has_extend_flag([_ | Rest]) -> has_extend_flag(Rest).

get_env_vars(Args) -> get_env_vars(Args, []).

get_env_vars([], Acc) -> lists:reverse(Acc);
get_env_vars(["-e", EnvVar | Rest], Acc) -> get_env_vars(Rest, [EnvVar | Acc]);
get_env_vars([_ | Rest], Acc) -> get_env_vars(Rest, Acc).

get_env_file([]) -> undefined;
get_env_file(["--env-file", EnvFile | _]) -> EnvFile;
get_env_file([_ | Rest]) -> get_env_file(Rest).

%% Simple JSON field extraction (works for simple string fields)
extract_json_field(Json, Field) ->
    Pattern = "\"" ++ Field ++ "\":\"",
    case string:str(Json, Pattern) of
        0 -> "";
        Pos ->
            Start = Pos + length(Pattern),
            Rest = lists:nthtail(Start - 1, Json),
            extract_until_quote(Rest)
    end.

extract_until_quote(Str) ->
    extract_until_quote(Str, []).

extract_until_quote([], Acc) ->
    lists:reverse(Acc);
extract_until_quote([$\" | _], Acc) ->
    lists:reverse(Acc);
extract_until_quote([$\\, $\" | Rest], Acc) ->
    extract_until_quote(Rest, [$\" | Acc]);
extract_until_quote([C | Rest], Acc) ->
    extract_until_quote(Rest, [C | Acc]).

%% Extract JSON array of strings from a field like "languages":["python","javascript",...]
extract_json_array(Json, Field) ->
    Pattern = "\"" ++ Field ++ "\":[",
    case string:str(Json, Pattern) of
        0 -> [];
        Pos ->
            Start = Pos + length(Pattern),
            Rest = lists:nthtail(Start - 1, Json),
            extract_array_elements(Rest, [])
    end.

extract_array_elements([], Acc) ->
    lists:reverse(Acc);
extract_array_elements([$] | _], Acc) ->
    lists:reverse(Acc);
extract_array_elements([$\" | Rest], Acc) ->
    {Elem, Remainder} = extract_until_quote_with_rest(Rest),
    extract_array_elements(Remainder, [Elem | Acc]);
extract_array_elements([_ | Rest], Acc) ->
    extract_array_elements(Rest, Acc).

extract_until_quote_with_rest(Str) ->
    extract_until_quote_with_rest(Str, []).

extract_until_quote_with_rest([], Acc) ->
    {lists:reverse(Acc), []};
extract_until_quote_with_rest([$\" | Rest], Acc) ->
    {lists:reverse(Acc), Rest};
extract_until_quote_with_rest([$\\, $\" | Rest], Acc) ->
    extract_until_quote_with_rest(Rest, [$\" | Acc]);
extract_until_quote_with_rest([C | Rest], Acc) ->
    extract_until_quote_with_rest(Rest, [C | Acc]).
