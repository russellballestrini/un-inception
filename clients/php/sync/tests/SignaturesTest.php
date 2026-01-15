<?php
/**
 * Tests for HMAC-SHA256 request signing
 */

declare(strict_types=1);

namespace Unsandbox\Tests;

use PHPUnit\Framework\TestCase;
use ReflectionClass;

require_once __DIR__ . '/../src/un.php';

use Unsandbox\Unsandbox;

class SignaturesTest extends TestCase
{
    private function invokePrivateMethod(object $object, string $methodName, array $parameters = [])
    {
        $reflection = new ReflectionClass(get_class($object));
        $method = $reflection->getMethod($methodName);
        $method->setAccessible(true);
        return $method->invokeArgs($object, $parameters);
    }

    public function testSignRequestBasic(): void
    {
        $client = new Unsandbox();
        $signature = $this->invokePrivateMethod($client, 'signRequest', [
            'my_secret',
            1234567890,
            'POST',
            '/execute',
            '{"language":"python"}'
        ]);

        // Signature should be 64 hex characters
        $this->assertEquals(64, strlen($signature));
        $this->assertMatchesRegularExpression('/^[0-9a-f]{64}$/', $signature);
    }

    public function testSignRequestGet(): void
    {
        $client = new Unsandbox();
        $signature = $this->invokePrivateMethod($client, 'signRequest', [
            'my_secret',
            1234567890,
            'GET',
            '/languages',
            null
        ]);

        $this->assertEquals(64, strlen($signature));
    }

    public function testSignRequestDelete(): void
    {
        $client = new Unsandbox();
        $signature = $this->invokePrivateMethod($client, 'signRequest', [
            'my_secret',
            1234567890,
            'DELETE',
            '/jobs/job_123',
            null
        ]);

        $this->assertEquals(64, strlen($signature));
    }

    public function testSignRequestDeterministic(): void
    {
        $client = new Unsandbox();

        $signature1 = $this->invokePrivateMethod($client, 'signRequest', [
            'test_secret',
            9999,
            'POST',
            '/test',
            '{"test":"data"}'
        ]);

        $signature2 = $this->invokePrivateMethod($client, 'signRequest', [
            'test_secret',
            9999,
            'POST',
            '/test',
            '{"test":"data"}'
        ]);

        $this->assertEquals($signature1, $signature2);
    }

    public function testSignRequestDifferentSecrets(): void
    {
        $client = new Unsandbox();

        $signature1 = $this->invokePrivateMethod($client, 'signRequest', [
            'secret1',
            1234567890,
            'POST',
            '/execute',
            'code'
        ]);

        $signature2 = $this->invokePrivateMethod($client, 'signRequest', [
            'secret2',
            1234567890,
            'POST',
            '/execute',
            'code'
        ]);

        $this->assertNotEquals($signature1, $signature2);
    }

    public function testSignRequestDifferentTimestamps(): void
    {
        $client = new Unsandbox();

        $signature1 = $this->invokePrivateMethod($client, 'signRequest', [
            'secret',
            1000,
            'POST',
            '/execute',
            'code'
        ]);

        $signature2 = $this->invokePrivateMethod($client, 'signRequest', [
            'secret',
            2000,
            'POST',
            '/execute',
            'code'
        ]);

        $this->assertNotEquals($signature1, $signature2);
    }

    public function testSignRequestDifferentPaths(): void
    {
        $client = new Unsandbox();

        $signature1 = $this->invokePrivateMethod($client, 'signRequest', [
            'secret',
            1234567890,
            'GET',
            '/jobs',
            null
        ]);

        $signature2 = $this->invokePrivateMethod($client, 'signRequest', [
            'secret',
            1234567890,
            'GET',
            '/languages',
            null
        ]);

        $this->assertNotEquals($signature1, $signature2);
    }

    public function testSignRequestDifferentMethods(): void
    {
        $client = new Unsandbox();

        $signature1 = $this->invokePrivateMethod($client, 'signRequest', [
            'secret',
            1234567890,
            'GET',
            '/jobs/123',
            null
        ]);

        $signature2 = $this->invokePrivateMethod($client, 'signRequest', [
            'secret',
            1234567890,
            'DELETE',
            '/jobs/123',
            null
        ]);

        $this->assertNotEquals($signature1, $signature2);
    }

    public function testSignRequestEmptyBody(): void
    {
        $client = new Unsandbox();

        $sig1 = $this->invokePrivateMethod($client, 'signRequest', [
            'secret',
            1234567890,
            'GET',
            '/test',
            ''
        ]);

        $sig2 = $this->invokePrivateMethod($client, 'signRequest', [
            'secret',
            1234567890,
            'GET',
            '/test',
            null
        ]);

        // Both should be the same (null and "" are both empty)
        $this->assertEquals($sig1, $sig2);
    }

    public function testSignRequestSpecialCharacters(): void
    {
        $client = new Unsandbox();
        $signature = $this->invokePrivateMethod($client, 'signRequest', [
            'my_secret',
            1234567890,
            'POST',
            '/execute',
            '{"code":"print(\\"hello\\")"}'
        ]);

        $this->assertEquals(64, strlen($signature));
        $this->assertIsString($signature);
    }

    public function testSignRequestMatchesPythonSdk(): void
    {
        // Verify the exact algorithm matches other SDKs
        // Message format: "timestamp:METHOD:path:body"
        $client = new Unsandbox();

        $secret = 'test_secret';
        $timestamp = 1609459200;
        $method = 'POST';
        $path = '/execute';
        $body = '{"language":"python"}';

        $signature = $this->invokePrivateMethod($client, 'signRequest', [
            $secret,
            $timestamp,
            $method,
            $path,
            $body
        ]);

        // Manually compute expected signature
        $message = "{$timestamp}:{$method}:{$path}:{$body}";
        $expected = hash_hmac('sha256', $message, $secret);

        $this->assertEquals($expected, $signature);
    }
}
