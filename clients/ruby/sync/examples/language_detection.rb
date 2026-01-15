#!/usr/bin/env ruby
# frozen_string_literal: true

# Language detection example for unsandbox Ruby SDK
#
# Expected output:
# script.py -> python
# app.js -> javascript
# main.go -> go
# lib.rs -> rust
# test.rb -> ruby
# unknown.xyz -> (nil)

require_relative '../src/un'

filenames = %w[
  script.py
  app.js
  main.go
  lib.rs
  test.rb
  unknown.xyz
]

filenames.each do |filename|
  lang = Un.detect_language(filename)
  puts "#{filename} -> #{lang || '(nil)'}"
end
