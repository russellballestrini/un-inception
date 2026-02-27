# frozen_string_literal: true

# This is free software for the public good of a permacomputer hosted at
# permacomputer.com, an always-on computer by the people, for the people.
# One which is durable, easy to repair, & distributed like tap water
# for machine learning intelligence.
#
# The permacomputer is community-owned infrastructure optimized around
# four values:
#
#   TRUTH      First principles, math & science, open source code freely distributed
#   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
#   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
#   LOVE       Be yourself without hurting others, cooperation through natural law
#
# This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
# Code is seeds to sprout on any abandoned technology.

# UN Ruby SDK - Functional Tests
#
# Tests library functions against real API.
# Requires: UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY
#
# Usage:
#   cd clients/ruby/sync && ruby test/test_functional.rb

$LOAD_PATH.unshift File.expand_path('../src', __dir__)

require 'minitest/autorun'
require 'un'

class TestFunctional < Minitest::Test
  def setup
    skip 'UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY required' unless credentials?
  end

  def test_health_check
    result = Un.health_check
    assert [true, false].include?(result), 'health_check should return boolean'
  end

  def test_validate_keys
    info = Un.validate_keys
    assert_kind_of Hash, info
    assert info.key?('valid') || info.key?(:valid), 'should have valid key'
    assert_equal true, (info['valid'] || info[:valid])
  end

  def test_get_languages
    langs = Un.get_languages
    assert_kind_of Array, langs
    refute_empty langs
    assert_includes langs, 'python'
  end

  def test_execute
    result = Un.execute_code('python', "print('hello from Ruby SDK')")
    assert_kind_of Hash, result
    stdout = result['stdout'] || result[:stdout] || ''
    assert_includes stdout, 'hello from Ruby SDK'
    exit_code = result['exit_code'] || result[:exit_code]
    assert_equal 0, exit_code
  end

  def test_execute_error
    result = Un.execute_code('python', 'import sys; sys.exit(1)')
    assert_kind_of Hash, result
    exit_code = result['exit_code'] || result[:exit_code]
    assert_equal 1, exit_code
  end

  def test_session_list
    sessions = Un.list_sessions
    assert_kind_of Array, sessions
  end

  def test_session_lifecycle
    session = Un.create_session('python')
    assert_kind_of Hash, session
    session_id = session['id'] || session[:id]
    refute_nil session_id, 'session should have id'

    Un.delete_session(session_id)
  end

  def test_service_list
    services = Un.list_services
    assert_kind_of Array, services
  end

  def test_snapshot_list
    snapshots = Un.list_snapshots
    assert_kind_of Array, snapshots
  end

  def test_image_list
    images = Un.list_images
    assert_kind_of Array, images
  end

  private

  def credentials?
    ENV['UNSANDBOX_PUBLIC_KEY'] && ENV['UNSANDBOX_SECRET_KEY'] &&
      !ENV['UNSANDBOX_PUBLIC_KEY'].empty? && !ENV['UNSANDBOX_SECRET_KEY'].empty?
  end
end
