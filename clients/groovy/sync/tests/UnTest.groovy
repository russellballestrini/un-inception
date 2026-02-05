#!/usr/bin/env groovy
// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
// Unit tests for Un SDK - Groovy Synchronous client

import groovy.test.GroovyTestCase

/**
 * Test suite for the Unsandbox Groovy SDK.
 *
 * Run with: groovy UnTest.groovy
 *
 * Integration tests require UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY
 * environment variables to be set.
 */
class UnTest extends GroovyTestCase {

    // Load the SDK
    static {
        def sdkPath = new File(UnTest.class.protectionDomain.codeSource.location.path).parentFile.parentFile
        evaluate(new File(sdkPath, 'src/un.groovy'))
    }

    // ========================================================================
    // Language Detection Tests
    // ========================================================================

    void testDetectPython() {
        assertEquals("python", detectLanguage("script.py"))
        assertEquals("python", detectLanguage("path/to/script.py"))
    }

    void testDetectJavaScript() {
        assertEquals("javascript", detectLanguage("app.js"))
    }

    void testDetectTypeScript() {
        assertEquals("typescript", detectLanguage("app.ts"))
    }

    void testDetectGo() {
        assertEquals("go", detectLanguage("main.go"))
    }

    void testDetectRust() {
        assertEquals("rust", detectLanguage("lib.rs"))
    }

    void testDetectJava() {
        assertEquals("java", detectLanguage("Main.java"))
    }

    void testDetectKotlin() {
        assertEquals("kotlin", detectLanguage("Main.kt"))
    }

    void testDetectGroovy() {
        assertEquals("groovy", detectLanguage("script.groovy"))
    }

    void testDetectCpp() {
        assertEquals("cpp", detectLanguage("main.cpp"))
    }

    void testDetectC() {
        assertEquals("c", detectLanguage("main.c"))
    }

    void testDetectRuby() {
        assertEquals("ruby", detectLanguage("script.rb"))
    }

    void testDetectPhp() {
        assertEquals("php", detectLanguage("index.php"))
    }

    void testDetectUnknown() {
        assertNull(detectLanguage("file.unknown"))
        assertNull(detectLanguage("noextension"))
    }

    // ========================================================================
    // Utility Function Tests
    // ========================================================================

    void testVersionString() {
        def ver = version()
        assertNotNull(ver)
        assertTrue("Version should be in X.Y.Z format", ver ==~ /\d+\.\d+\.\d+/)
    }

    void testHmacSignature() {
        def signature = hmacSign("secret", "message")
        assertNotNull(signature)
        assertEquals("HMAC-SHA256 should produce 64 hex chars", 64, signature.length())
        assertTrue("Signature should be lowercase hex", signature ==~ /[0-9a-f]+/)
    }

    void testHmacConsistent() {
        def sig1 = hmacSign("key", "data")
        def sig2 = hmacSign("key", "data")
        assertEquals("Same inputs should produce same signature", sig1, sig2)
    }

    void testHmacDifferent() {
        def sig1 = hmacSign("key1", "data")
        def sig2 = hmacSign("key2", "data")
        assertFalse("Different keys should produce different signatures", sig1 == sig2)
    }

    void testHmacKnownValue() {
        // HMAC-SHA256("key", "The quick brown fox jumps over the lazy dog")
        // Known value from various implementations
        def signature = hmacSign("key", "The quick brown fox jumps over the lazy dog")
        assertEquals("f7bc83f430538424b13298e6aa6fb143ef4d59a14946175997479dbc2d1a3cd8", signature)
    }

    // ========================================================================
    // Health Check Tests
    // ========================================================================

    void testHealthCheckReturnsBoolean() {
        def healthy = healthCheck()
        // We just verify it returns a boolean without throwing
        assertTrue(healthy instanceof Boolean)
    }

    // ========================================================================
    // Exception Tests
    // ========================================================================

    void testUnsandboxError() {
        def error = new UnsandboxError("Test message")
        assertEquals("Test message", error.message)
    }

    void testAuthenticationError() {
        def error = new AuthenticationError("Auth failed")
        assertEquals("Auth failed", error.message)
        assertTrue(error instanceof UnsandboxError)
    }

    void testExecutionError() {
        def error = new ExecutionError("Exec failed", 1, "stderr output")
        assertEquals("Exec failed", error.message)
        assertEquals(1, error.exitCode)
        assertEquals("stderr output", error.stderr)
    }

    void testAPIError() {
        def error = new APIError("API failed", 500, '{"error": "internal"}')
        assertEquals("API failed", error.message)
        assertEquals(500, error.statusCode)
        assertEquals('{"error": "internal"}', error.response)
    }

    void testTimeoutError() {
        def error = new TimeoutError("Operation timed out")
        assertEquals("Operation timed out", error.message)
        assertTrue(error instanceof UnsandboxError)
    }

    void testSudoChallengeError() {
        def error = new SudoChallengeError("challenge-123", '{"challenge_id": "challenge-123"}')
        assertEquals("challenge-123", error.challengeId)
        assertEquals('{"challenge_id": "challenge-123"}', error.responseBody)
    }

    // ========================================================================
    // Extension Map Tests
    // ========================================================================

    void testExtensionMapComplete() {
        // Verify the EXT_MAP has all expected extensions
        assertNotNull(EXT_MAP['.py'])
        assertNotNull(EXT_MAP['.js'])
        assertNotNull(EXT_MAP['.ts'])
        assertNotNull(EXT_MAP['.go'])
        assertNotNull(EXT_MAP['.rs'])
        assertNotNull(EXT_MAP['.java'])
        assertNotNull(EXT_MAP['.kt'])
        assertNotNull(EXT_MAP['.groovy'])
        assertNotNull(EXT_MAP['.rb'])
        assertNotNull(EXT_MAP['.php'])
        assertNotNull(EXT_MAP['.c'])
        assertNotNull(EXT_MAP['.cpp'])
        assertNotNull(EXT_MAP['.sh'])
        assertNotNull(EXT_MAP['.lua'])
        assertNotNull(EXT_MAP['.pl'])
    }

    // ========================================================================
    // Integration Tests (requires credentials)
    // ========================================================================

    void testExecutePythonCode() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def result = execute("python", 'print("Hello, World!")', [
            publicKey: publicKey,
            secretKey: secretKey
        ])

        assertNotNull(result)
        assertTrue(result.stdout?.contains("Hello, World!") ?: false)
    }

    void testExecuteJavaScriptCode() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def result = execute("javascript", 'console.log("Hello from JS")', [
            publicKey: publicKey,
            secretKey: secretKey
        ])

        assertNotNull(result)
        assertTrue(result.stdout?.contains("Hello from JS") ?: false)
    }

    void testGetLanguages() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def result = languages([
            publicKey: publicKey,
            secretKey: secretKey,
            forceRefresh: true
        ])

        assertNotNull(result)
        assertNotNull(result.languages)
        assertTrue(result.languages.size() > 0)
        assertTrue(result.languages.contains("python"))
        assertTrue(result.languages.contains("javascript"))
    }

    void testListJobs() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def jobs = listJobs([
            publicKey: publicKey,
            secretKey: secretKey
        ])

        assertNotNull(jobs)
        // Jobs list can be empty if no jobs are running
    }

    void testValidateKeys() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def result = validateKeys([
            publicKey: publicKey,
            secretKey: secretKey
        ])

        assertNotNull(result)
    }

    void testListSessions() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def sessions = sessionList([
            publicKey: publicKey,
            secretKey: secretKey
        ])

        assertNotNull(sessions)
    }

    void testListServices() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def services = serviceList([
            publicKey: publicKey,
            secretKey: secretKey
        ])

        assertNotNull(services)
    }

    void testListSnapshots() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def snapshots = snapshotList([
            publicKey: publicKey,
            secretKey: secretKey
        ])

        assertNotNull(snapshots)
    }

    void testListImages() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def images = imageList(null, [
            publicKey: publicKey,
            secretKey: secretKey
        ])

        assertNotNull(images)
    }

    void testAsyncExecution() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def job = executeAsync("python", 'print("Async test")', [
            publicKey: publicKey,
            secretKey: secretKey
        ])

        assertNotNull(job)
        assertNotNull(job.job_id)

        // Wait for completion
        def result = wait(job.job_id, [
            publicKey: publicKey,
            secretKey: secretKey,
            maxPolls: 30
        ])

        assertNotNull(result)
        assertTrue(result.stdout?.contains("Async test") ?: (result.result?.stdout?.contains("Async test") ?: false))
    }

    void testLogsFetch() {
        def publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY")
        def secretKey = System.getenv("UNSANDBOX_SECRET_KEY")

        if (!publicKey || !secretKey) {
            println "Skipping integration test - credentials not set"
            return
        }

        def result = logsFetch('all', 10, null, null, [
            publicKey: publicKey,
            secretKey: secretKey
        ])

        assertNotNull(result)
    }

    void testLogCallbackInterface() {
        // Verify LogCallback interface exists and can be implemented
        def callback = { source, line ->
            assertNotNull(source)
            assertNotNull(line)
        } as LogCallback

        assertNotNull(callback)
    }

    // ========================================================================
    // Run all tests
    // ========================================================================

    static void main(String[] args) {
        println "Running Unsandbox Groovy SDK Tests..."
        println "=" * 60

        def test = new UnTest()
        def methods = UnTest.class.declaredMethods.findAll {
            it.name.startsWith('test') && it.parameterCount == 0
        }

        int passed = 0
        int failed = 0
        int skipped = 0

        methods.each { method ->
            print "Testing ${method.name}... "
            try {
                method.invoke(test)
                println "PASS"
                passed++
            } catch (Exception e) {
                def cause = e.cause ?: e
                if (cause.message?.contains("Skipping")) {
                    println "SKIP"
                    skipped++
                } else {
                    println "FAIL: ${cause.message}"
                    failed++
                }
            }
        }

        println "=" * 60
        println "Results: ${passed} passed, ${failed} failed, ${skipped} skipped"

        if (failed > 0) {
            System.exit(1)
        }
    }
}
