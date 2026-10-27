<?php
/**
 * Target OS option (tickets #123, #124): "os" is sent only when given,
 * normalized to lowercase, and invalid values are rejected before any request.
 *
 * A local recording HTTP server (php -S with a router script) stands in for
 * our API via UNSANDBOX_API_URL, so no network and no credentials are used.
 */

declare(strict_types=1);

namespace Unsandbox\Tests;

use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/un.php';

use Unsandbox\Unsandbox;

class OsTest extends TestCase
{
    private static $server;
    private static string $dir;
    private static string $log;
    private static ?string $previousUrl = null;
    private Unsandbox $client;

    public static function setUpBeforeClass(): void
    {
        self::$dir = sys_get_temp_dir() . '/un-os-test-' . bin2hex(random_bytes(6));
        mkdir(self::$dir, 0700);
        self::$log = self::$dir . '/requests.jsonl';
        touch(self::$log);
        file_put_contents(self::$dir . '/router.php', <<<'PHP'
<?php
$path = parse_url($_SERVER['REQUEST_URI'], PHP_URL_PATH);
file_put_contents(__DIR__ . '/requests.jsonl', json_encode([
    'method' => $_SERVER['REQUEST_METHOD'],
    'path' => $path,
    'body' => file_get_contents('php://input'),
]) . "\n", FILE_APPEND | LOCK_EX);
header('Content-Type: application/json');
if (strpos($path, '/execute') === 0) {
    echo json_encode(['status' => 'completed', 'stdout' => '', 'job_id' => 'job-1']);
} elseif ($path === '/sessions') {
    echo json_encode(['session_id' => 's-1']);
} else {
    echo json_encode(['service_id' => 'v-1']);
}
PHP);

        $sock = stream_socket_server('tcp://127.0.0.1:0');
        $port = (int)substr(strrchr(stream_socket_get_name($sock, false), ':'), 1);
        fclose($sock);
        self::$server = proc_open(
            [PHP_BINARY, '-S', "127.0.0.1:{$port}", self::$dir . '/router.php'],
            [['pipe', 'r'], ['file', '/dev/null', 'w'], ['file', '/dev/null', 'w']],
            $pipes
        );
        for ($i = 0; $i < 50; $i++) {
            $conn = @fsockopen('127.0.0.1', $port);
            if ($conn) {
                fclose($conn);
                break;
            }
            usleep(100000);
        }
        $prev = getenv('UNSANDBOX_API_URL');
        self::$previousUrl = $prev === false ? null : $prev;
        putenv("UNSANDBOX_API_URL=http://127.0.0.1:{$port}");
    }

    public static function tearDownAfterClass(): void
    {
        if (self::$server) {
            proc_terminate(self::$server);
            proc_close(self::$server);
        }
        putenv(self::$previousUrl === null ? 'UNSANDBOX_API_URL' : 'UNSANDBOX_API_URL=' . self::$previousUrl);
        @unlink(self::$dir . '/router.php');
        @unlink(self::$log);
        @rmdir(self::$dir);
    }

    protected function setUp(): void
    {
        file_put_contents(self::$log, '');
        $this->client = new Unsandbox('pk-test', 'sk-test');
    }

    /** @return array<int, array{method: string, path: string, body: string}> */
    private function requests(): array
    {
        $lines = array_filter(explode("\n", (string)file_get_contents(self::$log)));
        return array_map(fn($l) => json_decode($l, true), array_values($lines));
    }

    private function bodyOf(string $path): array
    {
        foreach ($this->requests() as $r) {
            if ($r['method'] === 'POST' && $r['path'] === $path) {
                return json_decode($r['body'], true);
            }
        }
        $this->fail("no POST {$path} recorded");
    }

    public function testNormalizeOs(): void
    {
        $this->assertSame('linux', Unsandbox::normalizeOs('linux'));
        $this->assertSame('freebsd', Unsandbox::normalizeOs('FreeBSD'));
    }

    public function testNormalizeOsRejectsOtherValues(): void
    {
        $this->expectException(\InvalidArgumentException::class);
        Unsandbox::normalizeOs('openbsd');
    }

    public function testExecuteWithOs(): void
    {
        $this->client->executeCode('bash', 'echo hi', null, null, 'FreeBSD');
        $this->assertSame('freebsd', $this->bodyOf('/execute')['os']);
    }

    public function testExecuteWithoutOs(): void
    {
        $this->client->executeCode('bash', 'echo hi');
        $this->assertArrayNotHasKey('os', $this->bodyOf('/execute'));
    }

    public function testExecuteAsyncWithOs(): void
    {
        $this->client->executeAsync('bash', 'echo hi', null, null, 'linux');
        $this->assertSame('linux', $this->bodyOf('/execute')['os']);
    }

    public function testSessionWithAndWithoutOs(): void
    {
        $this->client->createSession('bash', null, null, ['os' => 'freebsd']);
        $this->assertSame('freebsd', $this->bodyOf('/sessions')['os']);
        file_put_contents(self::$log, '');
        $this->client->createSession('bash');
        $this->assertArrayNotHasKey('os', $this->bodyOf('/sessions'));
    }

    public function testServiceWithAndWithoutOs(): void
    {
        $this->client->createService('ostest', [80], 'true', null, null, ['os' => 'freebsd']);
        $this->assertSame('freebsd', $this->bodyOf('/services')['os']);
        file_put_contents(self::$log, '');
        $this->client->createService('ostest', [80], 'true');
        $this->assertArrayNotHasKey('os', $this->bodyOf('/services'));
    }

    public function testInvalidOsSendsNothing(): void
    {
        $calls = [
            fn() => $this->client->executeCode('bash', 'echo hi', null, null, 'openbsd'),
            fn() => $this->client->executeAsync('bash', 'echo hi', null, null, ''),
            fn() => $this->client->createSession('bash', null, null, ['os' => 'windows']),
            fn() => $this->client->createService('ostest', [80], 'true', null, null, ['os' => 'openbsd']),
        ];
        foreach ($calls as $call) {
            try {
                $call();
                $this->fail('expected InvalidArgumentException');
            } catch (\InvalidArgumentException $e) {
                $this->assertStringContainsString('expected linux or freebsd', $e->getMessage());
            }
        }
        $this->assertSame([], $this->requests());
    }
}
