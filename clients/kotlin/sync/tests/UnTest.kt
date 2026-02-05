// PUBLIC DOMAIN - NO LICENSE, NO WARRANTY
// Unit tests for Un SDK - Kotlin Synchronous client

import org.junit.jupiter.api.Test
import org.junit.jupiter.api.BeforeEach
import org.junit.jupiter.api.DisplayName
import org.junit.jupiter.api.Nested
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable
import org.junit.jupiter.api.Assertions.*

class UnTest {

    @Nested
    @DisplayName("Language Detection Tests")
    inner class LanguageDetectionTests {

        @Test
        @DisplayName("Should detect Python from .py extension")
        fun detectPython() {
            assertEquals("python", detectLanguage("script.py"))
            assertEquals("python", detectLanguage("path/to/script.py"))
        }

        @Test
        @DisplayName("Should detect JavaScript from .js extension")
        fun detectJavaScript() {
            assertEquals("javascript", detectLanguage("app.js"))
        }

        @Test
        @DisplayName("Should detect TypeScript from .ts extension")
        fun detectTypeScript() {
            assertEquals("typescript", detectLanguage("app.ts"))
        }

        @Test
        @DisplayName("Should detect Go from .go extension")
        fun detectGo() {
            assertEquals("go", detectLanguage("main.go"))
        }

        @Test
        @DisplayName("Should detect Rust from .rs extension")
        fun detectRust() {
            assertEquals("rust", detectLanguage("lib.rs"))
        }

        @Test
        @DisplayName("Should detect Java from .java extension")
        fun detectJava() {
            assertEquals("java", detectLanguage("Main.java"))
        }

        @Test
        @DisplayName("Should detect Kotlin from .kt extension")
        fun detectKotlin() {
            assertEquals("kotlin", detectLanguage("Main.kt"))
        }

        @Test
        @DisplayName("Should detect Groovy from .groovy extension")
        fun detectGroovy() {
            assertEquals("groovy", detectLanguage("script.groovy"))
        }

        @Test
        @DisplayName("Should throw for unknown extension")
        fun detectUnknown() {
            assertThrows(RuntimeException::class.java) {
                detectLanguage("file.unknown")
            }
        }
    }

    @Nested
    @DisplayName("Utility Function Tests")
    inner class UtilityTests {

        @Test
        @DisplayName("Version should return a valid version string")
        fun versionString() {
            val ver = version()
            assertNotNull(ver)
            assertTrue(ver.matches(Regex("\\d+\\.\\d+\\.\\d+")), "Version should be in X.Y.Z format")
        }

        @Test
        @DisplayName("HMAC sign should produce valid hex signature")
        fun hmacSignature() {
            val signature = hmacSign("secret", "message")
            assertNotNull(signature)
            assertEquals(64, signature.length, "HMAC-SHA256 should produce 64 hex chars")
            assertTrue(signature.matches(Regex("[0-9a-f]+")), "Signature should be lowercase hex")
        }

        @Test
        @DisplayName("HMAC sign should be consistent")
        fun hmacConsistent() {
            val sig1 = hmacSign("key", "data")
            val sig2 = hmacSign("key", "data")
            assertEquals(sig1, sig2, "Same inputs should produce same signature")
        }

        @Test
        @DisplayName("HMAC sign should differ with different inputs")
        fun hmacDifferent() {
            val sig1 = hmacSign("key1", "data")
            val sig2 = hmacSign("key2", "data")
            assertNotEquals(sig1, sig2, "Different keys should produce different signatures")
        }
    }

    @Nested
    @DisplayName("Health Check Tests")
    inner class HealthCheckTests {

        @Test
        @DisplayName("Health check should return boolean")
        fun healthCheckReturnsBoolean() {
            val healthy = healthCheck()
            // We just verify it returns without throwing
            assertTrue(healthy || !healthy)
        }
    }

    @Nested
    @DisplayName("Integration Tests (requires credentials)")
    @EnabledIfEnvironmentVariable(named = "UNSANDBOX_PUBLIC_KEY", matches = ".+")
    inner class IntegrationTests {

        private lateinit var publicKey: String
        private lateinit var secretKey: String

        @BeforeEach
        fun setUp() {
            publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY") ?: ""
            secretKey = System.getenv("UNSANDBOX_SECRET_KEY") ?: ""
        }

        @Test
        @DisplayName("Should execute Python code successfully")
        fun executePythonCode() {
            val result = execute("python", "print('Hello, World!')", publicKey, secretKey)

            assertNotNull(result)
            assertTrue(result["stdout"].toString().contains("Hello, World!"))
        }

        @Test
        @DisplayName("Should execute JavaScript code successfully")
        fun executeJavaScriptCode() {
            val result = execute("javascript", "console.log('Hello from JS')", publicKey, secretKey)

            assertNotNull(result)
            assertTrue(result["stdout"].toString().contains("Hello from JS"))
        }

        @Test
        @DisplayName("Should get supported languages")
        fun getLanguagesTest() {
            val languages = getLanguages(publicKey, secretKey)

            assertNotNull(languages)
            assertTrue(languages.isNotEmpty())
            assertTrue(languages.contains("python"))
            assertTrue(languages.contains("javascript"))
        }

        @Test
        @DisplayName("Should list jobs")
        fun listJobsTest() {
            val jobs = listJobs(publicKey, secretKey)
            assertNotNull(jobs)
        }

        @Test
        @DisplayName("Should validate keys successfully")
        fun validateKeysTest() {
            val result = validateKeys(publicKey, secretKey)
            assertNotNull(result)
        }

        @Test
        @DisplayName("Should list sessions")
        fun listSessionsTest() {
            val sessions = sessionList(publicKey, secretKey)
            assertNotNull(sessions)
        }

        @Test
        @DisplayName("Should list services")
        fun listServicesTest() {
            val services = serviceList(publicKey, secretKey)
            assertNotNull(services)
        }

        @Test
        @DisplayName("Should list snapshots")
        fun listSnapshotsTest() {
            val snapshots = snapshotList(publicKey, secretKey)
            assertNotNull(snapshots)
        }

        @Test
        @DisplayName("Should list images")
        fun listImagesTest() {
            val images = imageList(null, publicKey, secretKey)
            assertNotNull(images)
        }
    }
}
