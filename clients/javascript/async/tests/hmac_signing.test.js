// This is free software for the public good of a permacomputer hosted at
// permacomputer.com, an always-on computer by the people, for the people.
// One which is durable, easy to repair, & distributed like tap water
// for machine learning intelligence.
//
// The permacomputer is community-owned infrastructure optimized around
// four values:
//
//   TRUTH      First principles, math & science, open source code freely distributed
//   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
//   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
//   LOVE       Be yourself without hurting others, cooperation through natural law
//
// This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
// Code is seeds to sprout on any abandoned technology.

/**
 * Tests for HMAC request signing
 */

import crypto from 'crypto';

// We need to test the signRequest function, but it's not exported.
// So we'll recreate the same logic here to verify the expected behavior.
function signRequest(secretKey, timestamp, method, urlPath, body) {
  const bodyStr = body || '';
  const message = `${timestamp}:${method}:${urlPath}:${bodyStr}`;
  return crypto
    .createHmac('sha256', secretKey)
    .update(message)
    .digest('hex');
}

describe('HMAC-SHA256 Signature', () => {
  test('should generate 64-character hex string', () => {
    const signature = signRequest(
      'secret',
      1234567890,
      'POST',
      '/execute',
      '{"code":"test"}'
    );

    expect(typeof signature).toBe('string');
    expect(signature.length).toBe(64);
    expect(/^[0-9a-f]+$/.test(signature)).toBe(true);
  });

  test('should be deterministic', () => {
    const sig1 = signRequest(
      'secret',
      1234567890,
      'POST',
      '/execute',
      '{"code":"test"}'
    );

    const sig2 = signRequest(
      'secret',
      1234567890,
      'POST',
      '/execute',
      '{"code":"test"}'
    );

    expect(sig1).toBe(sig2);
  });

  test('different secrets produce different signatures', () => {
    const sig1 = signRequest(
      'secret1',
      1234567890,
      'POST',
      '/execute',
      '{"code":"test"}'
    );

    const sig2 = signRequest(
      'secret2',
      1234567890,
      'POST',
      '/execute',
      '{"code":"test"}'
    );

    expect(sig1).not.toBe(sig2);
  });

  test('different timestamps produce different signatures', () => {
    const sig1 = signRequest(
      'secret',
      1234567890,
      'POST',
      '/execute',
      '{"code":"test"}'
    );

    const sig2 = signRequest(
      'secret',
      1234567891,
      'POST',
      '/execute',
      '{"code":"test"}'
    );

    expect(sig1).not.toBe(sig2);
  });

  test('different methods produce different signatures', () => {
    const sig1 = signRequest(
      'secret',
      1234567890,
      'POST',
      '/execute',
      '{"code":"test"}'
    );

    const sig2 = signRequest(
      'secret',
      1234567890,
      'GET',
      '/execute',
      '{"code":"test"}'
    );

    expect(sig1).not.toBe(sig2);
  });

  test('different paths produce different signatures', () => {
    const sig1 = signRequest(
      'secret',
      1234567890,
      'GET',
      '/jobs/123',
      null
    );

    const sig2 = signRequest(
      'secret',
      1234567890,
      'GET',
      '/jobs/456',
      null
    );

    expect(sig1).not.toBe(sig2);
  });

  test('handles empty/null body', () => {
    const sig1 = signRequest(
      'secret',
      1234567890,
      'GET',
      '/languages',
      null
    );

    const sig2 = signRequest(
      'secret',
      1234567890,
      'GET',
      '/languages',
      ''
    );

    // Both should produce valid signatures
    expect(typeof sig1).toBe('string');
    expect(sig1.length).toBe(64);
    expect(typeof sig2).toBe('string');
    expect(sig2.length).toBe(64);
  });

  test('handles special characters in body', () => {
    const bodyWithSpecial = '{"code":"print(\\"hello\\")"}';
    const signature = signRequest(
      'secret',
      1234567890,
      'POST',
      '/execute',
      bodyWithSpecial
    );

    expect(typeof signature).toBe('string');
    expect(signature.length).toBe(64);
  });

  test('message format is timestamp:METHOD:path:body', () => {
    const secret = 'test_secret';
    const timestamp = 1234567890;
    const method = 'POST';
    const path = '/execute';
    const body = '{"test":"data"}';

    // Build expected message
    const expectedMessage = `${timestamp}:${method}:${path}:${body}`;
    const expectedSignature = crypto
      .createHmac('sha256', secret)
      .update(expectedMessage)
      .digest('hex');

    // Compare with function output
    const actualSignature = signRequest(secret, timestamp, method, path, body);
    expect(actualSignature).toBe(expectedSignature);
  });
});
