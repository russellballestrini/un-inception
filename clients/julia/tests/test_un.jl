#!/usr/bin/env julia
# Test suite for Julia Unsandbox SDK
# Run: julia tests/test_un.jl

using Test

# Add parent directory to path so we can include the SDK
push!(LOAD_PATH, joinpath(@__DIR__, "..", "sync", "src"))

# Include the SDK (don't run main)
include(joinpath(@__DIR__, "..", "sync", "src", "un.jl"))

# Test constants
@testset "Constants" begin
    @test API_BASE == "https://api.unsandbox.com"
    @test PORTAL_BASE == "https://unsandbox.com"
    @test LANGUAGES_CACHE_TTL == 3600
end

# Test language detection
@testset "Language Detection" begin
    @test detect_language("test.py") == "python"
    @test detect_language("test.js") == "javascript"
    @test detect_language("test.rb") == "ruby"
    @test detect_language("test.jl") == "julia"
    @test detect_language("test.go") == "go"
    @test detect_language("test.rs") == "rust"
    @test detect_language("test.f90") == "fortran"
    @test detect_language("test.cob") == "cobol"
    @test detect_language("test.pro") == "prolog"
    @test detect_language("test.unknown") == "unknown"
end

# Test HMAC signing
@testset "HMAC Signing" begin
    sig = hmac_sign("secret", "message")
    @test typeof(sig) == String
    @test length(sig) == 64  # SHA256 hex is 64 chars
    # Verify deterministic
    sig2 = hmac_sign("secret", "message")
    @test sig == sig2
end

# Test version
@testset "Version" begin
    v = version()
    @test typeof(v) == String
    @test v == VERSION
end

# Test health check (requires network)
@testset "Health Check" begin
    # Skip if no network
    if haskey(ENV, "SKIP_NETWORK_TESTS")
        @test_skip health_check()
    else
        result = health_check()
        @test typeof(result) == Bool
    end
end

# Test library function signatures (don't call API)
@testset "Library Function Signatures" begin
    # Execution functions
    @test hasmethod(execute, Tuple{String, String})
    @test hasmethod(execute_async, Tuple{String, String})
    @test hasmethod(wait_job, Tuple{String})
    @test hasmethod(get_job, Tuple{String})
    @test hasmethod(cancel_job, Tuple{String})
    @test hasmethod(list_jobs, Tuple{})
    @test hasmethod(get_languages, Tuple{})

    # Session functions
    @test hasmethod(session_list, Tuple{})
    @test hasmethod(session_get, Tuple{String})
    @test hasmethod(session_create, Tuple{})
    @test hasmethod(session_destroy, Tuple{String})
    @test hasmethod(session_freeze, Tuple{String})
    @test hasmethod(session_unfreeze, Tuple{String})
    @test hasmethod(session_boost, Tuple{String, Int})
    @test hasmethod(session_unboost, Tuple{String})
    @test hasmethod(session_execute, Tuple{String, String})

    # Service functions
    @test hasmethod(service_list, Tuple{})
    @test hasmethod(service_get, Tuple{String})
    @test hasmethod(service_create, Tuple{String})
    @test hasmethod(service_destroy, Tuple{String})
    @test hasmethod(service_freeze, Tuple{String})
    @test hasmethod(service_unfreeze, Tuple{String})
    @test hasmethod(service_lock, Tuple{String})
    @test hasmethod(service_unlock, Tuple{String})
    @test hasmethod(service_redeploy, Tuple{String})
    @test hasmethod(service_logs, Tuple{String})
    @test hasmethod(service_execute, Tuple{String, String})
    @test hasmethod(service_resize, Tuple{String, Int})
    @test hasmethod(service_env_get, Tuple{String})
    @test hasmethod(service_env_set, Tuple{String, String})
    @test hasmethod(service_env_delete, Tuple{String})
    @test hasmethod(service_env_export, Tuple{String})

    # Snapshot functions
    @test hasmethod(snapshot_list, Tuple{})
    @test hasmethod(snapshot_get, Tuple{String})
    @test hasmethod(snapshot_session, Tuple{String})
    @test hasmethod(snapshot_service, Tuple{String})
    @test hasmethod(snapshot_restore, Tuple{String})
    @test hasmethod(snapshot_delete, Tuple{String})
    @test hasmethod(snapshot_lock, Tuple{String})
    @test hasmethod(snapshot_unlock, Tuple{String})
    @test hasmethod(snapshot_clone, Tuple{String, String})

    # Image functions
    @test hasmethod(image_list, Tuple{})
    @test hasmethod(image_get, Tuple{String})
    @test hasmethod(image_publish, Tuple{String, String})
    @test hasmethod(image_delete, Tuple{String})
    @test hasmethod(image_lock, Tuple{String})
    @test hasmethod(image_unlock, Tuple{String})
    @test hasmethod(image_set_visibility, Tuple{String, String})
    @test hasmethod(image_grant_access, Tuple{String, String})
    @test hasmethod(image_revoke_access, Tuple{String, String})
    @test hasmethod(image_list_trusted, Tuple{String})
    @test hasmethod(image_transfer, Tuple{String, String})
    @test hasmethod(image_spawn, Tuple{String})
    @test hasmethod(image_clone, Tuple{String})

    # Utility functions
    @test hasmethod(validate_keys, Tuple{})
    @test hasmethod(logs_fetch, Tuple{String})
    @test hasmethod(health_check, Tuple{})
    @test hasmethod(version, Tuple{})
    @test hasmethod(last_error, Tuple{})
    @test hasmethod(hmac_sign, Tuple{String, String})
end

println("\nAll tests passed!")
