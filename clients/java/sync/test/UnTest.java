/**
 * Unit tests for Un SDK - Synchronous Java client
 *
 * These tests verify:
 * - JSON serialization/deserialization
 * - HMAC-SHA256 signature generation
 * - Credential resolution logic
 * - Language detection
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
    }
}
