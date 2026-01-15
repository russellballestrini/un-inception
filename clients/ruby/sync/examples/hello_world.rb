#!/usr/bin/env ruby
# frozen_string_literal: true

# Hello World example for unsandbox Ruby SDK
#
# Expected output (requires valid API credentials):
# {
#   "status": "completed",
#   "stdout": "Hello from unsandbox!\n",
#   "stderr": "",
#   "exit_code": 0
# }

require_relative '../src/un'

begin
  result = Un.execute_code('python', 'print("Hello from unsandbox!")')
  puts "Status: #{result['status']}"
  puts "Output: #{result['stdout']}"
rescue Un::CredentialsError => e
  puts "Credentials error: #{e.message}"
rescue Un::APIError => e
  puts "API error: #{e.message}"
  puts "Status code: #{e.status_code}" if e.status_code
end
