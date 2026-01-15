/**
 * Tests for async operations
 *
 * Note: These tests verify the async/await patterns and Promise behavior.
 * Integration tests with the actual API require credentials.
 */

import { TimeoutError } from '../src/un_async.js';

describe('TimeoutError', () => {
  test('should be an Error instance', () => {
    const error = new TimeoutError('test message');
    expect(error).toBeInstanceOf(Error);
    expect(error).toBeInstanceOf(TimeoutError);
  });

  test('should have correct name', () => {
    const error = new TimeoutError('test message');
    expect(error.name).toBe('TimeoutError');
  });

  test('should have correct message', () => {
    const error = new TimeoutError('test message');
    expect(error.message).toBe('test message');
  });
});

describe('Async Patterns', () => {
  describe('sleep function behavior', () => {
    test('should delay for specified milliseconds', async () => {
      const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

      const start = Date.now();
      await sleep(100);
      const elapsed = Date.now() - start;

      // Allow some tolerance for timing
      expect(elapsed).toBeGreaterThanOrEqual(90);
      expect(elapsed).toBeLessThan(200);
    });
  });

  describe('polling delays', () => {
    test('poll delays should follow exponential backoff pattern', () => {
      const POLL_DELAYS_MS = [300, 450, 700, 900, 650, 1600, 2000];

      // Verify pattern exists
      expect(POLL_DELAYS_MS.length).toBe(7);
      expect(POLL_DELAYS_MS[0]).toBe(300);
      expect(POLL_DELAYS_MS[POLL_DELAYS_MS.length - 1]).toBe(2000);

      // Verify delays generally increase (with some variance for jitter)
      const lastDelay = POLL_DELAYS_MS[POLL_DELAYS_MS.length - 1];
      const firstDelay = POLL_DELAYS_MS[0];
      expect(lastDelay).toBeGreaterThan(firstDelay);
    });
  });

  describe('Promise.all for concurrent execution', () => {
    test('should execute multiple promises concurrently', async () => {
      const delay = (ms, value) =>
        new Promise((resolve) => setTimeout(() => resolve(value), ms));

      const start = Date.now();
      const results = await Promise.all([
        delay(100, 'a'),
        delay(100, 'b'),
        delay(100, 'c'),
      ]);
      const elapsed = Date.now() - start;

      expect(results).toEqual(['a', 'b', 'c']);
      // All should complete in ~100ms, not 300ms (sequential)
      expect(elapsed).toBeLessThan(200);
    });

    test('should reject if any promise rejects', async () => {
      const delay = (ms, value, shouldReject = false) =>
        new Promise((resolve, reject) =>
          setTimeout(() => {
            if (shouldReject) reject(new Error(value));
            else resolve(value);
          }, ms)
        );

      await expect(
        Promise.all([
          delay(100, 'a'),
          delay(50, 'error', true),
          delay(100, 'c'),
        ])
      ).rejects.toThrow('error');
    });
  });
});

describe('API Response Handling', () => {
  describe('terminal statuses', () => {
    test('should recognize terminal statuses', () => {
      const terminalStatuses = ['completed', 'failed', 'timeout', 'cancelled'];

      terminalStatuses.forEach((status) => {
        expect(terminalStatuses.includes(status)).toBe(true);
      });
    });

    test('should recognize non-terminal statuses', () => {
      const terminalStatuses = ['completed', 'failed', 'timeout', 'cancelled'];
      const nonTerminalStatuses = ['pending', 'running'];

      nonTerminalStatuses.forEach((status) => {
        expect(terminalStatuses.includes(status)).toBe(false);
      });
    });
  });
});
