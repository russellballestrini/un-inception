#!/usr/bin/env ruby
# frozen_string_literal: true

# Async job pattern demonstration - standalone version
#
# This example demonstrates the async job pattern:
# 1. Submit a job
# 2. Poll for completion
# 3. Get results
#
# Expected output:
# === Async Job Pattern Demo ===
# Step 1: Submit job (would return job_id)
# Step 2: Poll status until complete
# Step 3: Retrieve results
# Pattern: submit -> poll -> retrieve
# Demo complete!

def main
  puts '=== Async Job Pattern Demo ==='

  puts 'Step 1: Submit job (would return job_id)'
  job_id = 'job-example-123'
  puts "  Simulated job_id: #{job_id}"

  puts 'Step 2: Poll status until complete'
  %w[queued running running completed].each_with_index do |status, i|
    puts "  Poll #{i + 1}: status=#{status}"
  end

  puts 'Step 3: Retrieve results'
  puts '  stdout: 42'
  puts '  exit_code: 0'

  puts "\nPattern: submit -> poll -> retrieve"
  puts 'Demo complete!'

  0
end

exit(main)
