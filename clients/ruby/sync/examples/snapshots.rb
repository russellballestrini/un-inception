#!/usr/bin/env ruby
# frozen_string_literal: true

# Snapshot operations example for unsandbox Ruby SDK
#
# Expected output (requires valid API credentials and active session):
# Creating session snapshot...
# Snapshot created: snap-abc123
# Listing snapshots...
# Found 1 snapshot(s)
#   - snap-abc123: my-backup

require_relative '../src/un'

# Note: You need an active session_id to create a snapshot
# This is typically obtained from session creation APIs
SESSION_ID = ENV['UNSANDBOX_SESSION_ID'] || 'your-session-id'

begin
  puts 'Creating session snapshot...'
  snapshot_id = Un.session_snapshot(SESSION_ID, name: 'my-backup')
  puts "Snapshot created: #{snapshot_id}"

  puts 'Listing snapshots...'
  snapshots = Un.list_snapshots
  puts "Found #{snapshots.length} snapshot(s)"
  snapshots.each do |snap|
    puts "  - #{snap['snapshot_id']}: #{snap['name']}"
  end
rescue Un::CredentialsError => e
  puts "Credentials error: #{e.message}"
rescue Un::APIError => e
  puts "API error: #{e.message}"
end
