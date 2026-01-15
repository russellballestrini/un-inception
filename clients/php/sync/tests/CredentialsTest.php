<?php
/**
 * Tests for 4-tier credential resolution
 */

declare(strict_types=1);

namespace Unsandbox\Tests;

use PHPUnit\Framework\TestCase;
use ReflectionClass;

require_once __DIR__ . '/../src/un.php';

use Unsandbox\Unsandbox;
use Unsandbox\CredentialsException;

class CredentialsTest extends TestCase
{
    private string $originalHome;
    private ?string $originalPublicKey;
    private ?string $originalSecretKey;
    private ?string $originalAccount;

    protected function setUp(): void
    {
        // Save original environment
        $this->originalHome = getenv('HOME') ?: '';
        $this->originalPublicKey = getenv('UNSANDBOX_PUBLIC_KEY') ?: null;
        $this->originalSecretKey = getenv('UNSANDBOX_SECRET_KEY') ?: null;
        $this->originalAccount = getenv('UNSANDBOX_ACCOUNT') ?: null;

        // Clear environment variables
        putenv('UNSANDBOX_PUBLIC_KEY');
        putenv('UNSANDBOX_SECRET_KEY');
        putenv('UNSANDBOX_ACCOUNT');
    }

    protected function tearDown(): void
    {
        // Restore original environment
        if ($this->originalHome) {
            putenv("HOME={$this->originalHome}");
        }
        if ($this->originalPublicKey !== null) {
            putenv("UNSANDBOX_PUBLIC_KEY={$this->originalPublicKey}");
        } else {
            putenv('UNSANDBOX_PUBLIC_KEY');
        }
        if ($this->originalSecretKey !== null) {
            putenv("UNSANDBOX_SECRET_KEY={$this->originalSecretKey}");
        } else {
            putenv('UNSANDBOX_SECRET_KEY');
        }
        if ($this->originalAccount !== null) {
            putenv("UNSANDBOX_ACCOUNT={$this->originalAccount}");
        } else {
            putenv('UNSANDBOX_ACCOUNT');
        }
    }

    private function invokePrivateMethod(object $object, string $methodName, array $parameters = [])
    {
        $reflection = new ReflectionClass(get_class($object));
        $method = $reflection->getMethod($methodName);
        $method->setAccessible(true);
        return $method->invokeArgs($object, $parameters);
    }

    public function testTier1MethodArguments(): void
    {
        $client = new Unsandbox();
        $creds = $this->invokePrivateMethod($client, 'resolveCredentials', [
            'pk_method',
            'sk_method'
        ]);

        $this->assertEquals(['pk_method', 'sk_method'], $creds);
    }

    public function testTier1ConstructorArguments(): void
    {
        $client = new Unsandbox('pk_constructor', 'sk_constructor');
        $creds = $this->invokePrivateMethod($client, 'resolveCredentials', [null, null]);

        $this->assertEquals(['pk_constructor', 'sk_constructor'], $creds);
    }

    public function testTier1MethodOverridesConstructor(): void
    {
        $client = new Unsandbox('pk_constructor', 'sk_constructor');
        $creds = $this->invokePrivateMethod($client, 'resolveCredentials', [
            'pk_method',
            'sk_method'
        ]);

        $this->assertEquals(['pk_method', 'sk_method'], $creds);
    }

    public function testTier2EnvironmentVariables(): void
    {
        putenv('UNSANDBOX_PUBLIC_KEY=pk_env');
        putenv('UNSANDBOX_SECRET_KEY=sk_env');

        $client = new Unsandbox();
        $creds = $this->invokePrivateMethod($client, 'resolveCredentials', [null, null]);

        $this->assertEquals(['pk_env', 'sk_env'], $creds);
    }

    public function testTier1OverridesTier2(): void
    {
        putenv('UNSANDBOX_PUBLIC_KEY=pk_env');
        putenv('UNSANDBOX_SECRET_KEY=sk_env');

        $client = new Unsandbox();
        $creds = $this->invokePrivateMethod($client, 'resolveCredentials', [
            'pk_method',
            'sk_method'
        ]);

        $this->assertEquals(['pk_method', 'sk_method'], $creds);
    }

    public function testNoCredentialsThrowsException(): void
    {
        // Set HOME to a temp directory without accounts.csv
        $tempDir = sys_get_temp_dir() . '/unsandbox_test_' . uniqid();
        mkdir($tempDir);
        putenv("HOME={$tempDir}");

        // Change to a directory without accounts.csv
        $cwd = getcwd();
        chdir($tempDir);

        try {
            $client = new Unsandbox();
            $this->expectException(CredentialsException::class);
            $this->invokePrivateMethod($client, 'resolveCredentials', [null, null]);
        } finally {
            chdir($cwd);
            rmdir($tempDir . '/.unsandbox');
            rmdir($tempDir);
        }
    }

    public function testCredentialsExceptionMessage(): void
    {
        $tempDir = sys_get_temp_dir() . '/unsandbox_test_' . uniqid();
        mkdir($tempDir);
        putenv("HOME={$tempDir}");

        $cwd = getcwd();
        chdir($tempDir);

        try {
            $client = new Unsandbox();
            $this->invokePrivateMethod($client, 'resolveCredentials', [null, null]);
            $this->fail('Expected CredentialsException');
        } catch (CredentialsException $e) {
            $this->assertStringContainsString('No credentials found', $e->getMessage());
            $this->assertStringContainsString('UNSANDBOX_PUBLIC_KEY', $e->getMessage());
            $this->assertStringContainsString('accounts.csv', $e->getMessage());
        } finally {
            chdir($cwd);
            rmdir($tempDir . '/.unsandbox');
            rmdir($tempDir);
        }
    }

    public function testPartialCredentialsMethodArguments(): void
    {
        // Only public key provided, should fall through to other tiers
        putenv('UNSANDBOX_PUBLIC_KEY=pk_env');
        putenv('UNSANDBOX_SECRET_KEY=sk_env');

        $client = new Unsandbox();
        $creds = $this->invokePrivateMethod($client, 'resolveCredentials', [
            'pk_method',
            null  // Missing secret key
        ]);

        // Should fall back to environment variables
        $this->assertEquals(['pk_env', 'sk_env'], $creds);
    }
}
