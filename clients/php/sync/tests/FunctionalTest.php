<?php
/**
 * This is free software for the public good of a permacomputer hosted at
 * permacomputer.com, an always-on computer by the people, for the people.
 * One which is durable, easy to repair, & distributed like tap water
 * for machine learning intelligence.
 *
 * The permacomputer is community-owned infrastructure optimized around
 * four values:
 *
 *   TRUTH      First principles, math & science, open source code freely distributed
 *   FREEDOM    Voluntary partnerships, freedom from tyranny & corporate control
 *   HARMONY    Minimal waste, self-renewing systems with diverse thriving connections
 *   LOVE       Be yourself without hurting others, cooperation through natural law
 *
 * This software contributes to that vision by enabling code execution across 42+ programming languages through a unified interface, accessible to all.
 * Code is seeds to sprout on any abandoned technology.
 *
 * UN PHP SDK - Functional Tests
 *
 * Tests library functions against real API.
 * Requires: UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY
 *
 * Usage:
 *   cd clients/php/sync && phpunit tests/FunctionalTest.php
 */

declare(strict_types=1);

namespace Unsandbox\Tests;

use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/un.php';

use Unsandbox\Unsandbox;

class FunctionalTest extends TestCase
{
    private Unsandbox $client;

    protected function setUp(): void
    {
        if (empty(getenv('UNSANDBOX_PUBLIC_KEY')) || empty(getenv('UNSANDBOX_SECRET_KEY'))) {
            $this->markTestSkipped('UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY required');
        }
        $this->client = new Unsandbox();
    }

    public function testHealthCheck(): void
    {
        $result = Unsandbox::healthCheck();
        $this->assertIsBool($result);
    }

    public function testValidateKeys(): void
    {
        $info = $this->client->validateKeys();
        $this->assertIsArray($info);
        $this->assertArrayHasKey('valid', $info);
        $this->assertTrue($info['valid']);
    }

    public function testGetLanguages(): void
    {
        $langs = $this->client->getLanguages();
        $this->assertIsArray($langs);
        $this->assertNotEmpty($langs);
        $this->assertContains('python', $langs);
    }

    public function testExecute(): void
    {
        $result = $this->client->executeCode('python', "print('hello from PHP SDK')");
        $this->assertIsArray($result);
        $this->assertStringContainsString('hello from PHP SDK', $result['stdout'] ?? '');
        $this->assertEquals(0, $result['exit_code'] ?? -1);
    }

    public function testExecuteError(): void
    {
        $result = $this->client->executeCode('python', 'import sys; sys.exit(1)');
        $this->assertIsArray($result);
        $this->assertEquals(1, $result['exit_code'] ?? -1);
    }

    public function testSessionList(): void
    {
        $sessions = $this->client->listSessions();
        $this->assertIsArray($sessions);
    }

    public function testSessionLifecycle(): void
    {
        $session = $this->client->createSession('python');
        $this->assertIsArray($session);
        $this->assertArrayHasKey('id', $session);
        $sessionId = $session['id'];

        $this->client->deleteSession($sessionId);
    }

    public function testServiceList(): void
    {
        $services = $this->client->listServices();
        $this->assertIsArray($services);
    }

    public function testSnapshotList(): void
    {
        $snapshots = $this->client->listSnapshots();
        $this->assertIsArray($snapshots);
    }

    public function testImageList(): void
    {
        $images = $this->client->listImages();
        $this->assertIsArray($images);
    }
}
