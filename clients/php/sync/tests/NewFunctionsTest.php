<?php
/**
 * Tests for new SDK functions (feature parity with C implementation)
 */

declare(strict_types=1);

namespace Unsandbox\Tests;

use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/un.php';

use Unsandbox\Unsandbox;

class NewFunctionsTest extends TestCase
{
    private Unsandbox $client;

    protected function setUp(): void
    {
        $this->client = new Unsandbox();
    }

    // =========================================================================
    // Utility Functions Tests
    // =========================================================================

    public function testVersionReturnsString(): void
    {
        $v = Unsandbox::version();
        $this->assertIsString($v);
        $this->assertNotEmpty($v);
    }

    public function testVersionIsSemanticFormat(): void
    {
        $v = Unsandbox::version();
        $parts = explode('.', $v);
        $this->assertGreaterThanOrEqual(2, count($parts), "Version should be semantic: $v");
    }

    public function testHmacSignReturns64HexChars(): void
    {
        $signature = Unsandbox::hmacSign('secret_key', 'message_to_sign');
        $this->assertIsString($signature);
        $this->assertEquals(64, strlen($signature));
        $this->assertMatchesRegularExpression('/^[0-9a-f]+$/', $signature);
    }

    public function testHmacSignIsDeterministic(): void
    {
        $sig1 = Unsandbox::hmacSign('secret', 'message');
        $sig2 = Unsandbox::hmacSign('secret', 'message');
        $this->assertEquals($sig1, $sig2);
    }

    public function testHmacSignDifferentSecretsDifferentSignatures(): void
    {
        $sig1 = Unsandbox::hmacSign('secret1', 'message');
        $sig2 = Unsandbox::hmacSign('secret2', 'message');
        $this->assertNotEquals($sig1, $sig2);
    }

    public function testHmacSignDifferentMessagesDifferentSignatures(): void
    {
        $sig1 = Unsandbox::hmacSign('secret', 'message1');
        $sig2 = Unsandbox::hmacSign('secret', 'message2');
        $this->assertNotEquals($sig1, $sig2);
    }

    public function testHmacSignMatchesPhpHashHmac(): void
    {
        $secret = 'test_secret';
        $message = '1234567890:POST:/execute:';
        $signature = Unsandbox::hmacSign($secret, $message);
        $expected = hash_hmac('sha256', $message, $secret);
        $this->assertEquals($expected, $signature);
    }

    public function testLastErrorReturnsNullOrString(): void
    {
        $error = Unsandbox::lastError();
        $this->assertTrue($error === null || is_string($error));
    }

    // =========================================================================
    // Method Exports Tests
    // =========================================================================

    public function testExecutionMethodsExist(): void
    {
        // 8 execution methods
        $this->assertTrue(method_exists($this->client, 'executeCode'));
        $this->assertTrue(method_exists($this->client, 'executeAsync'));
        $this->assertTrue(method_exists($this->client, 'getJob'));
        $this->assertTrue(method_exists($this->client, 'waitForJob'));
        $this->assertTrue(method_exists($this->client, 'cancelJob'));
        $this->assertTrue(method_exists($this->client, 'listJobs'));
        $this->assertTrue(method_exists($this->client, 'getLanguages'));
        $this->assertTrue(method_exists(Unsandbox::class, 'detectLanguage'));
    }

    public function testSessionMethodsExist(): void
    {
        // 9 session methods
        $this->assertTrue(method_exists($this->client, 'listSessions'));
        $this->assertTrue(method_exists($this->client, 'getSession'));
        $this->assertTrue(method_exists($this->client, 'createSession'));
        $this->assertTrue(method_exists($this->client, 'deleteSession'));
        $this->assertTrue(method_exists($this->client, 'freezeSession'));
        $this->assertTrue(method_exists($this->client, 'unfreezeSession'));
        $this->assertTrue(method_exists($this->client, 'boostSession'));
        $this->assertTrue(method_exists($this->client, 'unboostSession'));
        $this->assertTrue(method_exists($this->client, 'shellSession'));
    }

    public function testServiceMethodsExist(): void
    {
        // 17 service methods
        $this->assertTrue(method_exists($this->client, 'listServices'));
        $this->assertTrue(method_exists($this->client, 'createService'));
        $this->assertTrue(method_exists($this->client, 'getService'));
        $this->assertTrue(method_exists($this->client, 'updateService'));
        $this->assertTrue(method_exists($this->client, 'deleteService'));
        $this->assertTrue(method_exists($this->client, 'freezeService'));
        $this->assertTrue(method_exists($this->client, 'unfreezeService'));
        $this->assertTrue(method_exists($this->client, 'lockService'));
        $this->assertTrue(method_exists($this->client, 'unlockService'));
        $this->assertTrue(method_exists($this->client, 'setUnfreezeOnDemand'));
        $this->assertTrue(method_exists($this->client, 'getServiceLogs'));
        $this->assertTrue(method_exists($this->client, 'getServiceEnv'));
        $this->assertTrue(method_exists($this->client, 'setServiceEnv'));
        $this->assertTrue(method_exists($this->client, 'deleteServiceEnv'));
        $this->assertTrue(method_exists($this->client, 'exportServiceEnv'));
        $this->assertTrue(method_exists($this->client, 'redeployService'));
        $this->assertTrue(method_exists($this->client, 'executeInService'));
        $this->assertTrue(method_exists($this->client, 'resizeService'), 'resizeService should exist (NEW)');
    }

    public function testSnapshotMethodsExist(): void
    {
        // 9 snapshot methods
        $this->assertTrue(method_exists($this->client, 'sessionSnapshot'));
        $this->assertTrue(method_exists($this->client, 'serviceSnapshot'));
        $this->assertTrue(method_exists($this->client, 'listSnapshots'));
        $this->assertTrue(method_exists($this->client, 'getSnapshot'), 'getSnapshot should exist (NEW)');
        $this->assertTrue(method_exists($this->client, 'restoreSnapshot'));
        $this->assertTrue(method_exists($this->client, 'deleteSnapshot'));
        $this->assertTrue(method_exists($this->client, 'lockSnapshot'));
        $this->assertTrue(method_exists($this->client, 'unlockSnapshot'));
        $this->assertTrue(method_exists($this->client, 'cloneSnapshot'));
    }

    public function testImageMethodsExist(): void
    {
        // 13 image methods
        $this->assertTrue(method_exists($this->client, 'imagePublish'));
        $this->assertTrue(method_exists($this->client, 'listImages'));
        $this->assertTrue(method_exists($this->client, 'getImage'));
        $this->assertTrue(method_exists($this->client, 'deleteImage'));
        $this->assertTrue(method_exists($this->client, 'lockImage'));
        $this->assertTrue(method_exists($this->client, 'unlockImage'));
        $this->assertTrue(method_exists($this->client, 'setImageVisibility'));
        $this->assertTrue(method_exists($this->client, 'grantImageAccess'));
        $this->assertTrue(method_exists($this->client, 'revokeImageAccess'));
        $this->assertTrue(method_exists($this->client, 'listImageTrusted'));
        $this->assertTrue(method_exists($this->client, 'transferImage'));
        $this->assertTrue(method_exists($this->client, 'spawnFromImage'));
        $this->assertTrue(method_exists($this->client, 'cloneImage'));
    }

    public function testLogsMethodsExist(): void
    {
        // 2 PaaS logs methods (NEW)
        $this->assertTrue(method_exists($this->client, 'logsFetch'), 'logsFetch should exist (NEW)');
        $this->assertTrue(method_exists($this->client, 'logsStream'), 'logsStream should exist (NEW)');
    }

    public function testUtilityMethodsExist(): void
    {
        $this->assertTrue(method_exists($this->client, 'validateKeys'));
        $this->assertTrue(method_exists(Unsandbox::class, 'version'), 'version should exist (NEW)');
        $this->assertTrue(method_exists(Unsandbox::class, 'healthCheck'), 'healthCheck should exist (NEW)');
        $this->assertTrue(method_exists(Unsandbox::class, 'lastError'), 'lastError should exist (NEW)');
        $this->assertTrue(method_exists(Unsandbox::class, 'hmacSign'), 'hmacSign should exist (NEW)');
    }

    // =========================================================================
    // Language Detection Tests
    // =========================================================================

    public function testDetectLanguagePython(): void
    {
        $this->assertEquals('python', Unsandbox::detectLanguage('test.py'));
    }

    public function testDetectLanguageJavascript(): void
    {
        $this->assertEquals('javascript', Unsandbox::detectLanguage('test.js'));
    }

    public function testDetectLanguageTypescript(): void
    {
        $this->assertEquals('typescript', Unsandbox::detectLanguage('test.ts'));
    }

    public function testDetectLanguageRuby(): void
    {
        $this->assertEquals('ruby', Unsandbox::detectLanguage('test.rb'));
    }

    public function testDetectLanguageGo(): void
    {
        $this->assertEquals('go', Unsandbox::detectLanguage('test.go'));
    }

    public function testDetectLanguageRust(): void
    {
        $this->assertEquals('rust', Unsandbox::detectLanguage('test.rs'));
    }

    public function testDetectLanguageUnknownReturnsNull(): void
    {
        $this->assertNull(Unsandbox::detectLanguage('test.unknown'));
    }
}

/**
 * Functional tests that require API credentials
 */
class FunctionalAPITest extends TestCase
{
    private ?Unsandbox $client = null;

    protected function setUp(): void
    {
        if (!$this->credentialsAvailable()) {
            $this->markTestSkipped('UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY required');
        }
        $this->client = new Unsandbox();
    }

    private function credentialsAvailable(): bool
    {
        return getenv('UNSANDBOX_PUBLIC_KEY') && getenv('UNSANDBOX_SECRET_KEY');
    }

    public function testHealthCheck(): void
    {
        $result = Unsandbox::healthCheck();
        $this->assertIsBool($result);
    }

    public function testValidateKeys(): void
    {
        $result = $this->client->validateKeys();
        $this->assertIsArray($result);
    }

    public function testGetLanguages(): void
    {
        $languages = $this->client->getLanguages();
        $this->assertIsArray($languages);
        $this->assertContains('python', $languages);
    }

    public function testListSessions(): void
    {
        $sessions = $this->client->listSessions();
        $this->assertIsArray($sessions);
    }

    public function testListServices(): void
    {
        $services = $this->client->listServices();
        $this->assertIsArray($services);
    }

    public function testListSnapshots(): void
    {
        $snapshots = $this->client->listSnapshots();
        $this->assertIsArray($snapshots);
    }

    public function testListImages(): void
    {
        $images = $this->client->listImages();
        $this->assertIsArray($images);
    }

    public function testExecuteCode(): void
    {
        $result = $this->client->executeCode('python', 'print("hello")');
        $this->assertIsArray($result);
        $this->assertContains($result['status'] ?? '', ['completed', 'pending']);
    }
}
