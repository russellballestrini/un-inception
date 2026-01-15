<?php
/**
 * Tests for language caching functionality
 */

declare(strict_types=1);

namespace Unsandbox\Tests;

use PHPUnit\Framework\TestCase;
use ReflectionClass;

require_once __DIR__ . '/../src/un.php';

use Unsandbox\Unsandbox;

class CachingTest extends TestCase
{
    private string $tempDir;
    private string $originalHome;

    protected function setUp(): void
    {
        // Create a temp directory for test cache
        $this->tempDir = sys_get_temp_dir() . '/unsandbox_cache_test_' . uniqid();
        mkdir($this->tempDir);

        // Save and set HOME to temp directory
        $this->originalHome = getenv('HOME') ?: '';
        putenv("HOME={$this->tempDir}");
    }

    protected function tearDown(): void
    {
        // Restore HOME
        putenv("HOME={$this->originalHome}");

        // Clean up temp directory
        $this->removeDir($this->tempDir);
    }

    private function removeDir(string $dir): void
    {
        if (!is_dir($dir)) {
            return;
        }

        $files = array_diff(scandir($dir), ['.', '..']);
        foreach ($files as $file) {
            $path = $dir . '/' . $file;
            is_dir($path) ? $this->removeDir($path) : unlink($path);
        }
        rmdir($dir);
    }

    private function invokePrivateMethod(object $object, string $methodName, array $parameters = [])
    {
        $reflection = new ReflectionClass(get_class($object));
        $method = $reflection->getMethod($methodName);
        $method->setAccessible(true);
        return $method->invokeArgs($object, $parameters);
    }

    public function testCacheDirectoryCreated(): void
    {
        $client = new Unsandbox();

        // Trigger directory creation
        $this->invokePrivateMethod($client, 'getUnsandboxDir');

        $this->assertDirectoryExists($this->tempDir . '/.unsandbox');
    }

    public function testCachePathCorrect(): void
    {
        $client = new Unsandbox();
        $path = $this->invokePrivateMethod($client, 'getLanguagesCachePath');

        $this->assertEquals($this->tempDir . '/.unsandbox/languages.json', $path);
    }

    public function testLoadLanguagesCacheReturnsNullWhenMissing(): void
    {
        $client = new Unsandbox();
        $result = $this->invokePrivateMethod($client, 'loadLanguagesCache');

        $this->assertNull($result);
    }

    public function testSaveAndLoadLanguagesCache(): void
    {
        $client = new Unsandbox();
        $languages = ['python', 'javascript', 'go'];

        $this->invokePrivateMethod($client, 'saveLanguagesCache', [$languages]);
        $loaded = $this->invokePrivateMethod($client, 'loadLanguagesCache');

        $this->assertEquals($languages, $loaded);
    }

    public function testCacheExpiry(): void
    {
        $client = new Unsandbox();
        $languages = ['python', 'javascript'];

        $this->invokePrivateMethod($client, 'saveLanguagesCache', [$languages]);

        // Modify file time to be older than TTL (1 hour)
        $cachePath = $this->invokePrivateMethod($client, 'getLanguagesCachePath');
        touch($cachePath, time() - 3700);  // 1 hour + 100 seconds ago

        $loaded = $this->invokePrivateMethod($client, 'loadLanguagesCache');

        $this->assertNull($loaded);
    }

    public function testCacheFreshWithinTtl(): void
    {
        $client = new Unsandbox();
        $languages = ['python', 'javascript', 'ruby'];

        $this->invokePrivateMethod($client, 'saveLanguagesCache', [$languages]);

        // Modify file time to be just within TTL
        $cachePath = $this->invokePrivateMethod($client, 'getLanguagesCachePath');
        touch($cachePath, time() - 3500);  // 1 hour - 100 seconds ago

        $loaded = $this->invokePrivateMethod($client, 'loadLanguagesCache');

        $this->assertEquals($languages, $loaded);
    }

    public function testCacheJsonFormat(): void
    {
        $client = new Unsandbox();
        $languages = ['python', 'javascript'];

        $this->invokePrivateMethod($client, 'saveLanguagesCache', [$languages]);

        $cachePath = $this->invokePrivateMethod($client, 'getLanguagesCachePath');
        $content = file_get_contents($cachePath);
        $data = json_decode($content, true);

        $this->assertIsArray($data);
        $this->assertArrayHasKey('languages', $data);
        $this->assertArrayHasKey('timestamp', $data);
        $this->assertEquals($languages, $data['languages']);
        $this->assertIsInt($data['timestamp']);
    }

    public function testLoadInvalidJsonReturnsNull(): void
    {
        $client = new Unsandbox();

        // Create directory and invalid cache file
        $this->invokePrivateMethod($client, 'getUnsandboxDir');
        $cachePath = $this->invokePrivateMethod($client, 'getLanguagesCachePath');
        file_put_contents($cachePath, 'not valid json');

        $loaded = $this->invokePrivateMethod($client, 'loadLanguagesCache');

        $this->assertNull($loaded);
    }

    public function testLoadMissingLanguagesKeyReturnsNull(): void
    {
        $client = new Unsandbox();

        // Create directory and cache file without 'languages' key
        $this->invokePrivateMethod($client, 'getUnsandboxDir');
        $cachePath = $this->invokePrivateMethod($client, 'getLanguagesCachePath');
        file_put_contents($cachePath, json_encode(['timestamp' => time()]));

        $loaded = $this->invokePrivateMethod($client, 'loadLanguagesCache');

        $this->assertNull($loaded);
    }
}
