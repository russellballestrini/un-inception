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
 * Tests for new SDK functions (feature parity with C implementation)
 */

import { createRequire } from 'module';
import crypto from 'crypto';

// Since un.js uses top-level await, we need to import dynamically
let un;

beforeAll(async () => {
  un = await import('../src/un.js');
});

describe('Utility Functions', () => {
  describe('sdkVersion', () => {
    test('should return a string', () => {
      const v = un.sdkVersion();
      expect(typeof v).toBe('string');
      expect(v.length).toBeGreaterThan(0);
    });

    test('should be semantic version format', () => {
      const v = un.sdkVersion();
      const parts = v.split('.');
      expect(parts.length).toBeGreaterThanOrEqual(2);
    });
  });

  describe('hmacSign', () => {
    test('should produce 64-character hex string', async () => {
      const signature = await un.hmacSign('secret_key', 'message_to_sign');
      expect(typeof signature).toBe('string');
      expect(signature.length).toBe(64);
      // Should be lowercase hex
      expect(/^[0-9a-f]+$/.test(signature)).toBe(true);
    });

    test('should be deterministic', async () => {
      const sig1 = await un.hmacSign('secret', 'message');
      const sig2 = await un.hmacSign('secret', 'message');
      expect(sig1).toBe(sig2);
    });

    test('should produce different signatures for different secrets', async () => {
      const sig1 = await un.hmacSign('secret1', 'message');
      const sig2 = await un.hmacSign('secret2', 'message');
      expect(sig1).not.toBe(sig2);
    });

    test('should produce different signatures for different messages', async () => {
      const sig1 = await un.hmacSign('secret', 'message1');
      const sig2 = await un.hmacSign('secret', 'message2');
      expect(sig1).not.toBe(sig2);
    });

    test('should match Node.js crypto HMAC', async () => {
      const secret = 'test_secret';
      const message = '1234567890:POST:/execute:';
      const signature = await un.hmacSign(secret, message);
      const expected = crypto.createHmac('sha256', secret).update(message).digest('hex');
      expect(signature).toBe(expected);
    });
  });

  describe('lastError', () => {
    test('should return null or string', () => {
      const error = un.lastError();
      expect(error === null || typeof error === 'string').toBe(true);
    });
  });
});

describe('Function Exports', () => {
  describe('Execution functions (8)', () => {
    test('executeCode is exported', () => {
      expect(typeof un.executeCode).toBe('function');
    });

    test('executeAsync is exported', () => {
      expect(typeof un.executeAsync).toBe('function');
    });

    test('getJob is exported', () => {
      expect(typeof un.getJob).toBe('function');
    });

    test('waitForJob is exported', () => {
      expect(typeof un.waitForJob).toBe('function');
    });

    test('cancelJob is exported', () => {
      expect(typeof un.cancelJob).toBe('function');
    });

    test('listJobs is exported', () => {
      expect(typeof un.listJobs).toBe('function');
    });

    test('getLanguages is exported', () => {
      expect(typeof un.getLanguages).toBe('function');
    });

    test('detectLanguage is exported', () => {
      expect(typeof un.detectLanguage).toBe('function');
    });
  });

  describe('Session functions (9)', () => {
    test('listSessions is exported', () => {
      expect(typeof un.listSessions).toBe('function');
    });

    test('getSession is exported', () => {
      expect(typeof un.getSession).toBe('function');
    });

    test('createSession is exported', () => {
      expect(typeof un.createSession).toBe('function');
    });

    test('deleteSession is exported', () => {
      expect(typeof un.deleteSession).toBe('function');
    });

    test('freezeSession is exported', () => {
      expect(typeof un.freezeSession).toBe('function');
    });

    test('unfreezeSession is exported', () => {
      expect(typeof un.unfreezeSession).toBe('function');
    });

    test('boostSession is exported', () => {
      expect(typeof un.boostSession).toBe('function');
    });

    test('unboostSession is exported', () => {
      expect(typeof un.unboostSession).toBe('function');
    });

    test('shellSession is exported', () => {
      expect(typeof un.shellSession).toBe('function');
    });
  });

  describe('Service functions (17)', () => {
    test('listServices is exported', () => {
      expect(typeof un.listServices).toBe('function');
    });

    test('createService is exported', () => {
      expect(typeof un.createService).toBe('function');
    });

    test('getService is exported', () => {
      expect(typeof un.getService).toBe('function');
    });

    test('updateService is exported', () => {
      expect(typeof un.updateService).toBe('function');
    });

    test('deleteService is exported', () => {
      expect(typeof un.deleteService).toBe('function');
    });

    test('freezeService is exported', () => {
      expect(typeof un.freezeService).toBe('function');
    });

    test('unfreezeService is exported', () => {
      expect(typeof un.unfreezeService).toBe('function');
    });

    test('lockService is exported', () => {
      expect(typeof un.lockService).toBe('function');
    });

    test('unlockService is exported', () => {
      expect(typeof un.unlockService).toBe('function');
    });

    test('setUnfreezeOnDemand is exported', () => {
      expect(typeof un.setUnfreezeOnDemand).toBe('function');
    });

    test('getServiceLogs is exported', () => {
      expect(typeof un.getServiceLogs).toBe('function');
    });

    test('getServiceEnv is exported', () => {
      expect(typeof un.getServiceEnv).toBe('function');
    });

    test('setServiceEnv is exported', () => {
      expect(typeof un.setServiceEnv).toBe('function');
    });

    test('deleteServiceEnv is exported', () => {
      expect(typeof un.deleteServiceEnv).toBe('function');
    });

    test('exportServiceEnv is exported', () => {
      expect(typeof un.exportServiceEnv).toBe('function');
    });

    test('redeployService is exported', () => {
      expect(typeof un.redeployService).toBe('function');
    });

    test('executeInService is exported', () => {
      expect(typeof un.executeInService).toBe('function');
    });

    test('resizeService is exported (NEW)', () => {
      expect(typeof un.resizeService).toBe('function');
    });
  });

  describe('Snapshot functions (9)', () => {
    test('sessionSnapshot is exported', () => {
      expect(typeof un.sessionSnapshot).toBe('function');
    });

    test('serviceSnapshot is exported', () => {
      expect(typeof un.serviceSnapshot).toBe('function');
    });

    test('listSnapshots is exported', () => {
      expect(typeof un.listSnapshots).toBe('function');
    });

    test('getSnapshot is exported (NEW)', () => {
      expect(typeof un.getSnapshot).toBe('function');
    });

    test('restoreSnapshot is exported', () => {
      expect(typeof un.restoreSnapshot).toBe('function');
    });

    test('deleteSnapshot is exported', () => {
      expect(typeof un.deleteSnapshot).toBe('function');
    });

    test('lockSnapshot is exported', () => {
      expect(typeof un.lockSnapshot).toBe('function');
    });

    test('unlockSnapshot is exported', () => {
      expect(typeof un.unlockSnapshot).toBe('function');
    });

    test('cloneSnapshot is exported', () => {
      expect(typeof un.cloneSnapshot).toBe('function');
    });
  });

  describe('Image functions (13)', () => {
    test('imagePublish is exported', () => {
      expect(typeof un.imagePublish).toBe('function');
    });

    test('listImages is exported', () => {
      expect(typeof un.listImages).toBe('function');
    });

    test('getImage is exported', () => {
      expect(typeof un.getImage).toBe('function');
    });

    test('deleteImage is exported', () => {
      expect(typeof un.deleteImage).toBe('function');
    });

    test('lockImage is exported', () => {
      expect(typeof un.lockImage).toBe('function');
    });

    test('unlockImage is exported', () => {
      expect(typeof un.unlockImage).toBe('function');
    });

    test('setImageVisibility is exported', () => {
      expect(typeof un.setImageVisibility).toBe('function');
    });

    test('grantImageAccess is exported', () => {
      expect(typeof un.grantImageAccess).toBe('function');
    });

    test('revokeImageAccess is exported', () => {
      expect(typeof un.revokeImageAccess).toBe('function');
    });

    test('listImageTrusted is exported', () => {
      expect(typeof un.listImageTrusted).toBe('function');
    });

    test('transferImage is exported', () => {
      expect(typeof un.transferImage).toBe('function');
    });

    test('spawnFromImage is exported', () => {
      expect(typeof un.spawnFromImage).toBe('function');
    });

    test('cloneImage is exported', () => {
      expect(typeof un.cloneImage).toBe('function');
    });
  });

  describe('PaaS Logs functions (2)', () => {
    test('logsFetch is exported (NEW)', () => {
      expect(typeof un.logsFetch).toBe('function');
    });

    test('logsStream is exported (NEW)', () => {
      expect(typeof un.logsStream).toBe('function');
    });
  });

  describe('Utility functions', () => {
    test('validateKeys is exported', () => {
      expect(typeof un.validateKeys).toBe('function');
    });

    test('sdkVersion is exported (NEW)', () => {
      expect(typeof un.sdkVersion).toBe('function');
    });

    test('healthCheck is exported (NEW)', () => {
      expect(typeof un.healthCheck).toBe('function');
    });

    test('lastError is exported (NEW)', () => {
      expect(typeof un.lastError).toBe('function');
    });

    test('hmacSign is exported (NEW)', () => {
      expect(typeof un.hmacSign).toBe('function');
    });
  });
});

describe('Language Detection', () => {
  test('detects Python files', () => {
    expect(un.detectLanguage('test.py')).toBe('python');
  });

  test('detects JavaScript files', () => {
    expect(un.detectLanguage('test.js')).toBe('javascript');
  });

  test('detects TypeScript files', () => {
    expect(un.detectLanguage('test.ts')).toBe('typescript');
  });

  test('detects Ruby files', () => {
    expect(un.detectLanguage('test.rb')).toBe('ruby');
  });

  test('detects Go files', () => {
    expect(un.detectLanguage('test.go')).toBe('go');
  });

  test('detects Rust files', () => {
    expect(un.detectLanguage('test.rs')).toBe('rust');
  });

  test('returns null for unknown extensions', () => {
    expect(un.detectLanguage('test.unknown')).toBeNull();
  });
});

// Functional tests require API credentials
const hasCredentials = process.env.UNSANDBOX_PUBLIC_KEY && process.env.UNSANDBOX_SECRET_KEY;

(hasCredentials ? describe : describe.skip)('Functional API Tests', () => {
  test('healthCheck returns boolean', async () => {
    const result = await un.healthCheck();
    expect(typeof result).toBe('boolean');
  });

  test('validateKeys returns object', async () => {
    const result = await un.validateKeys();
    expect(typeof result).toBe('object');
  });

  test('getLanguages returns array with python', async () => {
    const languages = await un.getLanguages();
    expect(Array.isArray(languages)).toBe(true);
    expect(languages).toContain('python');
  });

  test('listSessions returns array', async () => {
    const sessions = await un.listSessions();
    expect(Array.isArray(sessions)).toBe(true);
  });

  test('listServices returns array', async () => {
    const services = await un.listServices();
    expect(Array.isArray(services)).toBe(true);
  });

  test('listSnapshots returns array', async () => {
    const snapshots = await un.listSnapshots();
    expect(Array.isArray(snapshots)).toBe(true);
  });

  test('listImages returns array', async () => {
    const images = await un.listImages();
    expect(Array.isArray(images)).toBe(true);
  });

  test('executeCode returns result', async () => {
    const result = await un.executeCode('python', 'print("hello")');
    expect(typeof result).toBe('object');
    expect(['completed', 'pending']).toContain(result.status);
  });
});
