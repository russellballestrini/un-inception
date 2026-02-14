#!/usr/bin/env ruby
# frozen_string_literal: true

# Hello World example - standalone version
#
# This example demonstrates basic execution patterns.
# Shows how to execute code (simulated).
#
# To run:
#     ruby hello_world.rb
#
# Expected output:
#     Status: completed
#     Output: Hello from unsandbox!

# Simulated result (would normally call API)
result = {
  'status' => 'completed',
  'stdout' => "Hello from unsandbox!\n",
  'stderr' => '',
  'exit_code' => 0
}

puts "Status: #{result['status']}"
puts "Output: #{result['stdout'].strip}"
