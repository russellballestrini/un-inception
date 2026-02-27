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
 * UN Java SDK - Functional Tests
 *
 * Tests library functions against real API.
 * Requires: UNSANDBOX_PUBLIC_KEY, UNSANDBOX_SECRET_KEY
 *
 * Usage:
 *   javac -cp src tests/TestFunctional.java && java -cp src:tests TestFunctional
 */

import java.io.IOException;
import java.util.*;

public class TestFunctional {

    private static int passed = 0;
    private static int failed = 0;

    private static final String GREEN = "\033[32m";
    private static final String RED = "\033[31m";
    private static final String NC = "\033[0m";

    private static void check(boolean condition, String msg) {
        if (condition) {
            System.out.println("  " + GREEN + "✓" + NC + " " + msg);
            passed++;
        } else {
            System.out.println("  " + RED + "✗" + NC + " " + msg);
            failed++;
        }
    }

    private static void testHealthCheck() {
        System.out.println("\nTesting healthCheck()...");
        boolean result = Un.healthCheck();
        check(true, "healthCheck completed without exception");
    }

    private static void testValidateKeys() throws IOException {
        System.out.println("\nTesting validateKeys()...");
        Map<String, Object> info = Un.validateKeys(null, null);
        check(info != null, "validateKeys returns non-null");
        if (info != null) {
            check(info.containsKey("valid"), "result has 'valid' key");
            check(Boolean.TRUE.equals(info.get("valid")), "keys are valid");
            Object tier = info.get("tier");
            if (tier != null) System.out.println("    tier: " + tier);
        }
    }

    private static void testGetLanguages() throws IOException {
        System.out.println("\nTesting getLanguages()...");
        List<String> langs = Un.getLanguages(null, null);
        check(langs != null, "getLanguages returns non-null");
        if (langs != null) {
            check(!langs.isEmpty(), "at least one language returned");
            check(langs.contains("python"), "python is in languages list");
            System.out.println("    Found " + langs.size() + " languages");
        }
    }

    private static void testExecute() throws IOException {
        System.out.println("\nTesting executeCode()...");
        Map<String, Object> result = Un.executeCode("python", "print('hello from Java SDK')", null, null);
        check(result != null, "execute returns non-null");
        if (result != null) {
            String stdout = (String) result.get("stdout");
            check(stdout != null && stdout.contains("hello from Java SDK"), "stdout contains expected output");
            Object exitCode = result.get("exit_code");
            check(exitCode != null && ((Number) exitCode).intValue() == 0, "exit code is 0");
        }
    }

    private static void testExecuteError() throws IOException {
        System.out.println("\nTesting executeCode() with error...");
        Map<String, Object> result = Un.executeCode("python", "import sys; sys.exit(1)", null, null);
        check(result != null, "execute returns non-null");
        if (result != null) {
            Object exitCode = result.get("exit_code");
            check(exitCode != null && ((Number) exitCode).intValue() == 1, "exit code is 1");
        }
    }

    private static void testSessionList() throws IOException {
        System.out.println("\nTesting listSessions()...");
        List<Map<String, Object>> sessions = Un.listSessions(null, null);
        check(sessions != null, "listSessions returns non-null");
        if (sessions != null) {
            System.out.println("    Found " + sessions.size() + " sessions");
        }
    }

    private static void testSessionLifecycle() throws IOException {
        System.out.println("\nTesting session lifecycle (create, destroy)...");
        Map<String, Object> session = Un.createSession("python", null, null, null);
        check(session != null, "createSession returns non-null");
        if (session != null) {
            String sessionId = (String) session.get("id");
            check(sessionId != null, "session has id");
            System.out.println("    session_id: " + sessionId);

            if (sessionId != null) {
                Un.deleteSession(sessionId, null, null);
                check(true, "deleteSession completed");
            }
        }
    }

    private static void testServiceList() throws IOException {
        System.out.println("\nTesting listServices()...");
        List<Map<String, Object>> services = Un.listServices(null, null);
        check(services != null, "listServices returns non-null");
        if (services != null) {
            System.out.println("    Found " + services.size() + " services");
        }
    }

    private static void testSnapshotList() throws IOException {
        System.out.println("\nTesting listSnapshots()...");
        List<Map<String, Object>> snapshots = Un.listSnapshots(null, null);
        check(snapshots != null, "listSnapshots returns non-null");
        if (snapshots != null) {
            System.out.println("    Found " + snapshots.size() + " snapshots");
        }
    }

    private static void testImageList() throws IOException {
        System.out.println("\nTesting listImages()...");
        List<Map<String, Object>> images = Un.listImages(null, null, null);
        check(images != null, "listImages returns non-null");
        if (images != null) {
            System.out.println("    Found " + images.size() + " images");
        }
    }

    public static void main(String[] args) {
        System.out.println("=====================================");
        System.out.println("UN Java SDK - Functional Tests");
        System.out.println("Testing against real API");
        System.out.println("=====================================");

        String pk = System.getenv("UNSANDBOX_PUBLIC_KEY");
        String sk = System.getenv("UNSANDBOX_SECRET_KEY");

        if (pk == null || sk == null || pk.isEmpty() || sk.isEmpty()) {
            System.out.println("\n\033[33mSKIP: UNSANDBOX_PUBLIC_KEY and UNSANDBOX_SECRET_KEY not set\033[0m");
            System.exit(0);
        }

        try { testHealthCheck(); } catch (Exception e) { System.out.println("  " + RED + "✗ " + e.getMessage() + NC); failed++; }
        try { testValidateKeys(); } catch (Exception e) { System.out.println("  " + RED + "✗ " + e.getMessage() + NC); failed++; }
        try { testGetLanguages(); } catch (Exception e) { System.out.println("  " + RED + "✗ " + e.getMessage() + NC); failed++; }
        try { testExecute(); } catch (Exception e) { System.out.println("  " + RED + "✗ " + e.getMessage() + NC); failed++; }
        try { testExecuteError(); } catch (Exception e) { System.out.println("  " + RED + "✗ " + e.getMessage() + NC); failed++; }
        try { testSessionList(); } catch (Exception e) { System.out.println("  " + RED + "✗ " + e.getMessage() + NC); failed++; }
        try { testSessionLifecycle(); } catch (Exception e) { System.out.println("  " + RED + "✗ " + e.getMessage() + NC); failed++; }
        try { testServiceList(); } catch (Exception e) { System.out.println("  " + RED + "✗ " + e.getMessage() + NC); failed++; }
        try { testSnapshotList(); } catch (Exception e) { System.out.println("  " + RED + "✗ " + e.getMessage() + NC); failed++; }
        try { testImageList(); } catch (Exception e) { System.out.println("  " + RED + "✗ " + e.getMessage() + NC); failed++; }

        System.out.println("\n=====================================");
        System.out.println("Test Summary");
        System.out.println("=====================================");
        System.out.println("Passed: " + GREEN + passed + NC);
        System.out.println("Failed: " + RED + failed + NC);
        System.out.println("=====================================");

        System.exit(failed > 0 ? 1 : 0);
    }
}
