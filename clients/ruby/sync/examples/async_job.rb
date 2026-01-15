#!/usr/bin/env ruby
# frozen_string_literal: true

# Async job example for unsandbox Ruby SDK
#
# Expected output (requires valid API credentials):
# Job submitted: job-abc123
# Waiting for completion...
# Status: completed
# Output: 42

require_relative '../src/un'

begin
  # Submit an async job
  job_id = Un.execute_async('python', <<~PYTHON)
    import time
    time.sleep(2)
    print(42)
  PYTHON

  puts "Job submitted: #{job_id}"
  puts 'Waiting for completion...'

  # Wait for the job to complete
  result = Un.wait_for_job(job_id, timeout: 60)

  puts "Status: #{result['status']}"
  puts "Output: #{result['stdout']}"
rescue Un::CredentialsError => e
  puts "Credentials error: #{e.message}"
rescue Un::APIError => e
  puts "API error: #{e.message}"
end
