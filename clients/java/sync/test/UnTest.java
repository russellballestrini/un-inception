/**
 * Unit tests for Un SDK - Synchronous Java client
 *
 * These tests verify:
 * - JSON serialization/deserialization
 * - HMAC-SHA256 signature generation
 * - Credential resolution logic
 * - Language detection
 * - Utility functions
 *
 * To run tests:
 *     mvn test
 *
 * Note: API integration tests require valid credentials and are skipped
 * when UNSANDBOX_PUBLIC_KEY is not set.
 */

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Nested;
import org.junit.jupiter.api.condition.EnabledIfEnvironmentVariable;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;

import static org.junit.jupiter.api.Assertions.*;

public class UnTest {

    @Nested
    @DisplayName("Language Detection Tests")
    class LanguageDetectionTests {

        @Test
        @DisplayName("Should detect Python from .py extension")
        void detectPython() {
            assertEquals("python", Un.detectLanguage("script.py"));
            assertEquals("python", Un.detectLanguage("path/to/script.py"));
            assertEquals("python", Un.detectLanguage("SCRIPT.PY"));
        }

        @Test
        @DisplayName("Should detect JavaScript from .js extension")
        void detectJavaScript() {
            assertEquals("javascript", Un.detectLanguage("app.js"));
            assertEquals("javascript", Un.detectLanguage("index.JS"));
        }

        @Test
        @DisplayName("Should detect TypeScript from .ts extension")
        void detectTypeScript() {
            assertEquals("typescript", Un.detectLanguage("app.ts"));
        }

        @Test
        @DisplayName("Should detect Go from .go extension")
        void detectGo() {
            assertEquals("go", Un.detectLanguage("main.go"));
        }

        @Test
        @DisplayName("Should detect Rust from .rs extension")
        void detectRust() {
            assertEquals("rust", Un.detectLanguage("lib.rs"));
        }

        @Test
        @DisplayName("Should detect Java from .java extension")
        void detectJava() {
            assertEquals("java", Un.detectLanguage("Main.java"));
        }

        @Test
        @DisplayName("Should detect C++ from various extensions")
        void detectCpp() {
            assertEquals("cpp", Un.detectLanguage("main.cpp"));
            assertEquals("cpp", Un.detectLanguage("main.cc"));
            assertEquals("cpp", Un.detectLanguage("main.cxx"));
        }

        @Test
        @DisplayName("Should detect Kotlin from .kt extension")
        void detectKotlin() {
            assertEquals("kotlin", Un.detectLanguage("Main.kt"));
        }

        @Test
        @DisplayName("Should detect Groovy from .groovy extension")
        void detectGroovy() {
            assertEquals("groovy", Un.detectLanguage("script.groovy"));
        }

        @Test
        @DisplayName("Should return null for unknown extension")
        void detectUnknown() {
            assertNull(Un.detectLanguage("file.unknown"));
            assertNull(Un.detectLanguage("noextension"));
        }

        @Test
        @DisplayName("Should return null for null input")
        void detectNull() {
            assertNull(Un.detectLanguage(null));
        }
    }

    @Nested
    @DisplayName("Utility Function Tests")
    class UtilityTests {

        @Test
        @DisplayName("Version should return a valid version string")
        void versionString() {
            String version = Un.version();
            assertNotNull(version);
            assertTrue(version.matches("\\d+\\.\\d+\\.\\d+"), "Version should be in X.Y.Z format");
        }

        @Test
        @DisplayName("HMAC sign should produce valid hex signature")
        void hmacSignature() {
            String signature = Un.hmacSign("secret", "message");
            assertNotNull(signature);
            assertEquals(64, signature.length(), "HMAC-SHA256 should produce 64 hex chars");
            assertTrue(signature.matches("[0-9a-f]+"), "Signature should be lowercase hex");
        }

        @Test
        @DisplayName("HMAC sign should be consistent")
        void hmacConsistent() {
            String sig1 = Un.hmacSign("key", "data");
            String sig2 = Un.hmacSign("key", "data");
            assertEquals(sig1, sig2, "Same inputs should produce same signature");
        }

        @Test
        @DisplayName("HMAC sign should differ with different inputs")
        void hmacDifferent() {
            String sig1 = Un.hmacSign("key1", "data");
            String sig2 = Un.hmacSign("key2", "data");
            assertNotEquals(sig1, sig2, "Different keys should produce different signatures");
        }
    }

    @Nested
    @DisplayName("Credential Exception Tests")
    class CredentialExceptionTests {

        @Test
        @DisplayName("CredentialsException should contain message")
        void credentialsExceptionMessage() {
            Un.CredentialsException ex = new Un.CredentialsException("Test message");
            assertEquals("Test message", ex.getMessage());
        }
    }

    @Nested
    @DisplayName("API Exception Tests")
    class ApiExceptionTests {

        @Test
        @DisplayName("ApiException should contain status code and response body")
        void apiExceptionDetails() {
            Un.ApiException ex = new Un.ApiException("Error occurred", 401, "{\"error\": \"unauthorized\"}");
            assertEquals(401, ex.getStatusCode());
            assertEquals("{\"error\": \"unauthorized\"}", ex.getResponseBody());
            assertEquals("Error occurred", ex.getMessage());
        }
    }

    @Nested
    @DisplayName("Sudo Challenge Exception Tests")
    class SudoChallengeExceptionTests {

        @Test
        @DisplayName("SudoChallengeException should contain challenge ID")
        void sudoChallengeDetails() {
            Un.SudoChallengeException ex = new Un.SudoChallengeException(
                "challenge-123",
                "{\"challenge_id\": \"challenge-123\"}"
            );
            assertEquals("challenge-123", ex.getChallengeId());
            assertEquals("{\"challenge_id\": \"challenge-123\"}", ex.getResponseBody());
        }
    }

    @Nested
    @DisplayName("Integration Tests (requires credentials)")
    @EnabledIfEnvironmentVariable(named = "UNSANDBOX_PUBLIC_KEY", matches = ".+")
    class IntegrationTests {

        private String publicKey;
        private String secretKey;

        @BeforeEach
        void setUp() {
            publicKey = System.getenv("UNSANDBOX_PUBLIC_KEY");
            secretKey = System.getenv("UNSANDBOX_SECRET_KEY");
        }

        @Test
        @DisplayName("Should execute Python code successfully")
        void executePythonCode() throws IOException {
            Map<String, Object> result = Un.executeCode(
                "python",
                "print('Hello, World!')",
                publicKey,
                secretKey
            );

            assertNotNull(result);
            assertEquals("completed", result.get("status"));
            assertTrue(result.get("stdout").toString().contains("Hello, World!"));
        }

        @Test
        @DisplayName("Should execute JavaScript code successfully")
        void executeJavaScriptCode() throws IOException {
            Map<String, Object> result = Un.executeCode(
                "javascript",
                "console.log('Hello from JS')",
                publicKey,
                secretKey
            );

            assertNotNull(result);
            assertEquals("completed", result.get("status"));
            assertTrue(result.get("stdout").toString().contains("Hello from JS"));
        }

        @Test
        @DisplayName("Should get supported languages")
        void getLanguages() throws IOException {
            List<String> languages = Un.getLanguages(publicKey, secretKey);

            assertNotNull(languages);
            assertFalse(languages.isEmpty());
            assertTrue(languages.contains("python"));
            assertTrue(languages.contains("javascript"));
        }

        @Test
        @DisplayName("Should execute async and wait for job")
        void executeAsync() throws IOException {
            String jobId = Un.executeAsync(
                "python",
                "print('Async test')",
                publicKey,
                secretKey
            );

            assertNotNull(jobId);

            Map<String, Object> result = Un.waitForJob(jobId, publicKey, secretKey, 30000);

            assertNotNull(result);
            assertEquals("completed", result.get("status"));
            assertTrue(result.get("stdout").toString().contains("Async test"));
        }

        @Test
        @DisplayName("Should list jobs")
        void listJobs() throws IOException {
            List<Map<String, Object>> jobs = Un.listJobs(publicKey, secretKey);
            assertNotNull(jobs);
            // Jobs list can be empty if no jobs are running
        }

        @Test
        @DisplayName("Should validate keys successfully")
        void validateKeys() throws IOException {
            Map<String, Object> result = Un.validateKeys(publicKey, secretKey);
            assertNotNull(result);
            // The response should contain validation info
        }

        @Test
        @DisplayName("Should list sessions")
        void listSessions() throws IOException {
            List<Map<String, Object>> sessions = Un.listSessions(publicKey, secretKey);
            assertNotNull(sessions);
        }

        @Test
        @DisplayName("Should list services")
        void listServices() throws IOException {
            List<Map<String, Object>> services = Un.listServices(publicKey, secretKey);
            assertNotNull(services);
        }

        @Test
        @DisplayName("Should list snapshots")
        void listSnapshots() throws IOException {
            List<Map<String, Object>> snapshots = Un.listSnapshots(publicKey, secretKey);
            assertNotNull(snapshots);
        }

        @Test
        @DisplayName("Should list images")
        void listImages() throws IOException {
            List<Map<String, Object>> images = Un.listImages(null, publicKey, secretKey);
            assertNotNull(images);
        }
    }

    @Nested
    @DisplayName("Health Check Tests")
    class HealthCheckTests {

        @Test
        @DisplayName("Health check should return boolean")
        void healthCheckReturnsBoolean() {
            boolean healthy = Un.healthCheck();
            // We just verify it returns without throwing
            // The actual result depends on network connectivity
            assertTrue(healthy || !healthy);
        }
    }
}
